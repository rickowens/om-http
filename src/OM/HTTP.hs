{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Miscellaneous HTTP Utilities. -}
module OM.HTTP (
  runTlsRedirect,
  hstsDirective,
  requestLogging,
  setServer,
  insertResponseHeaderIfMissing,
  overwriteResponseHeader,
  staticSite,
  logExceptionsAndContinue,
) where


import Control.Concurrent (threadDelay)
import Control.Exception.Safe (tryAny, SomeException, throwM)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (Loc, LogSource, LogLevel, LogStr,
   MonadLoggerIO, logError, runLoggingT, logInfo)
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime, diffUTCTime)
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Data.Version (Version, showVersion)
import Network.HTTP.Types (urlEncode, Header, movedPermanently301,
   internalServerError500, Status, statusCode, statusMessage)
import Network.Wai (Middleware, Application, Response, ResponseReceived,
   mapResponseHeaders, responseLBS, responseStatus, requestMethod,
   rawPathInfo, rawQueryString)
import Network.Wai.Handler.Warp (run)
import OM.HTTP.StaticSite (staticSite)
import OM.Show (showt)
import qualified Data.ByteString.Lazy as BSL


{- |
  Runs a web server on port 80, that redirects to the given url. Does
  request logging, and sets the HSTS Directive header, and in the unlikely
  event of excptions it will also catch and log them.
-}
runTlsRedirect 
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) {- ^ Logging backend. -}
  -> ByteString {- ^ Server name. -}
  -> Version {- ^ Server version. -}
  -> ByteString {- ^ Target URL. -}
  -> IO ()
runTlsRedirect logging serverName serverVersion url =
  run 80
    . requestLogging logging
    . setServer serverName serverVersion
    . hstsDirective 600
    . logExceptionsAndContinue logging
    $ tlsRedirect url


{- |
  Inject the HSTS directives, see
  https://en.wikipedia.org/wiki/HTTP_Strict_Transport_Security.
-}
hstsDirective :: NominalDiffTime -> Middleware
hstsDirective age = insertResponseHeaderIfMissing header
  where
    header :: Header
    header =
      ("Strict-Transport-Security", "max-age=" <> showt (truncate age :: Int))


{- | Insert a response header only if it has not already been inserted. -}
insertResponseHeaderIfMissing :: Header -> Middleware
insertResponseHeaderIfMissing (name, val) app req respond =
    app req (respond . mapResponseHeaders doInsert)
  where
    doInsert :: [Header] -> [Header]
    doInsert headers
      | name `elem` (fst <$> headers) = headers
      | otherwise = (name, val):headers


{- |
  TLS redirect. An 'Application' that redirects unsecured requests to
  the secure HTTPS site.
-}
tlsRedirect :: ByteString -> Application
tlsRedirect url _req respond = respond $
  responseLBS
    movedPermanently301
    [
      ("Location", urlEncode False url),
      ("Content-Type", "text/html")
    ]
    (
      "<html>\
        \<head>\
          \<title>Owens Murray, LLC.</title>\
        \</head>\
        \<body>\
          \Please use our secure site,\
          \<a href=\"" <> BSL.fromStrict (urlEncode False url) <> "\">here</a>\
        \</body>\
      \</html>"
    )


{- | Set the @Server:@ header. -}
setServer :: ByteString -> Version -> Middleware
setServer serviceName version =
    overwriteResponseHeader ("Server", serverValue)
  where
    {- | The value of the @Server:@ header. -}
    serverValue = serviceName <> "/" <> fromString (showVersion version)


{- |
  Inserts a response header, clobbering any and all existing values for
  the given header.
-}
overwriteResponseHeader :: Header -> Middleware
overwriteResponseHeader (name, value) app req respond =
    app req (respond . mapResponseHeaders go)
  where
    go :: [Header] -> [Header]
    go headers =
      (name, value) : filter ((/= name) . fst) headers


{- | Logs an HTTP requst the OM way. -}
requestLogging
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Middleware
requestLogging logging app req respond =
    (`runLoggingT` logging) $ do
      $(logInfo) $ "Starting request: " <> reqStr
      liftIO . app req . loggingRespond =<< liftIO getCurrentTime
  where
    {- | Delegate to the underlying responder, and do some logging. -}
    loggingRespond :: UTCTime -> Response -> IO ResponseReceived
    loggingRespond start response = (`runLoggingT` logging) $ do
      {-
        Execute the underlying responder first so we get an accurate
        measurement of the request duration.
      -}
      ack <- liftIO $ respond response
      now <- liftIO getCurrentTime
      $(logInfo)
        $ reqStr <> " --> " <> showStatus (responseStatus response)
        <> " (" <> showt (diffUTCTime now start) <> ")"
      return ack

    {- | A Text representation of the request, suitable for logging. -}
    reqStr :: Text
    reqStr = decodeUtf8
      $ requestMethod req <> " " <> rawPathInfo req <> rawQueryString req

    {- |
      @instance Show Status@ shows the Haskell structure, which is
      not suitable for logging.
    -}
    showStatus :: Status -> Text
    showStatus stat =
      (showt . statusCode) stat <> " " <> (decodeUtf8 . statusMessage) stat


{- |
  Logs all exceptions, and returns a 500 Internal Server error. 

  This is useful because your wai framework won't always do what you
  expect when it encounters random exceptions. For instance, an exception
  thrown in IO may cause functionality of higher-level middlewares to be
  bypassed unless they know how to catch and re-throw exceptions (making
  them more complicated). This middleware explicitly will not re-throw
  exceptions, unless those exceptions were encountered after the headers
  have already been sent, e.g. when using 'Network.Wai.StreamingBody'.
  
  What it will do is generate a unique id for the exception and print
  that ID, so you can easily find it in the logs.
-}
logExceptionsAndContinue
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) {- ^ Logging backend. -}
  -> Middleware
logExceptionsAndContinue logging app req respond = (`runLoggingT` logging) $
    tryAny (liftIO (app req loggingRespond)) >>= \case
      Right ack -> return ack
      Left err -> do
        uuid <- logProblem err
        liftIO $ respond (errResponse uuid)

  where
    errResponse :: UUID -> Response
    errResponse uuid =
      responseLBS
        internalServerError500
        [("Content-Type", "text/plain")] 
        ("Internal Server Error. Error ID: " <> showt uuid)

    getUUID :: (MonadIO m) => m UUID
    getUUID = liftIO nextUUID >>= \case
      Nothing -> liftIO (threadDelay 1000) >> getUUID
      Just uuid -> return uuid

    loggingRespond :: Response -> IO ResponseReceived
    loggingRespond response = (`runLoggingT` logging) $
      tryAny (liftIO (respond response)) >>= \case
        Right ack -> return ack
        Left err -> do
          void $ logProblem err
          throwM err

    logProblem :: (MonadLoggerIO m) => SomeException -> m UUID
    logProblem err = do
      uuid <- getUUID
      $(logError)
        $ "Internal Server Error [" <> showt uuid <> "]: "
        <> showt err
      return uuid


