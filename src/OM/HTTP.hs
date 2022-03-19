{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
  sshConnect,
  staticPage,
  defaultIndex,
  BearerToken(..),
  staticMarkdown,
) where


import Prelude (Bool(False, True), Either(Left, Right), Eq((/=), (==)),
  Foldable(elem, foldMap, foldr), Functor(fmap), Maybe(Just, Nothing),
  Monad((>>), (>>=), return), MonadFail(fail), Monoid(mempty), Ord((<=)),
  RealFrac(truncate), Semigroup((<>)), Show(show), Traversable(mapM),
  ($), (++), (.), (<$>), (=<<), FilePath, IO, Int, String, break, concat,
  drop, filter, fst, id, mapM_, otherwise, putStrLn, undefined, zip)

import CMarkGFM (ListType(BULLET_LIST, ORDERED_LIST), Node(Node),
  NodeType(BLOCK_QUOTE, CODE, CODE_BLOCK, CUSTOM_BLOCK, CUSTOM_INLINE,
  DOCUMENT, EMPH, HEADING, HTML_BLOCK, HTML_INLINE, IMAGE, ITEM,
  LINEBREAK, LINK, LIST, PARAGRAPH, SOFTBREAK, STRIKETHROUGH, STRONG,
  TABLE, TABLE_CELL, TABLE_ROW, TEXT, THEMATIC_BREAK), PosInfo(PosInfo),
  Level, commonmarkToNode, listType)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Exception.Safe (SomeException, bracket, finally, throwM,
  tryAny)
import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr,
  MonadLoggerIO, logError, logInfo, runLoggingT)
import Data.ByteString (ByteString)
import Data.List ((\\), intercalate)
import Data.Maybe (catMaybes)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Data.Version (Version, showVersion)
import Language.Haskell.TH (Exp(AppE, LitE, VarE), Lit(StringL), Q,
  TExp, runIO)
import Language.Haskell.TH.Syntax (TExp(TExp), addDependentFile)
import Network.HTTP.Types (Header, Status, internalServerError500,
  methodNotAllowed405, movedPermanently301, ok200, statusCode,
  statusMessage)
import Network.Mime (defaultMimeLookup)
import Network.Socket (AddrInfo(addrAddress), Family(AF_INET),
  SocketType(Stream), Socket, close, connect, defaultProtocol,
  getAddrInfo, socket)
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai (Application, Middleware, Response, ResponseReceived,
  mapResponseHeaders, pathInfo, rawPathInfo, rawQueryString,
  requestMethod, responseLBS, responseRaw, responseStatus)
import Network.Wai.Handler.Warp (run)
import OM.Show (showt)
import Servant.API (ToHttpApiData, toUrlPiece)
import System.Directory (getDirectoryContents)
import System.FilePath.Posix ((</>), combine)
import System.Posix.Files (getFileStatus, isDirectory, isRegularFile)
import Text.Blaze.Html5 ((!), Attribute, AttributeValue, Html, a, br,
  code, div, em, hr, img, li, ol, p, pre, strong, text, toValue, ul)
import Text.Blaze.Html5.Attributes (class_, href, src)
import Text.Blaze.Renderer.String (renderMarkup)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Blaze.Html5.Attributes as HTML


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
      ("Location", url),
      ("Content-Type", "text/html")
    ]
    (
      "<html>\
        \<head>\
        \</head>\
        \<body>\
          \Please use our secure site,\
          \<a href=\"" <> BSL.fromStrict url <> "\">here</a>\
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


{- |
  Logs an HTTP request by emitting two log messages. The first messages
  logs that the request has begun. The second messages logs the status
  result and timing of the request once it is finished.

  > Starting request: GET /foo
  > GET /foo --> 200 Ok (0.001s)

  This can help debugging requests that hang or crash for whatever reason.
-}
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


{- |
  'Middleware' that provides an HTTP @CONNECT@ passthrough to the local
  ssh port. Useful primarily for bypassing content-inspection firewalls.
-}
sshConnect :: Middleware
sshConnect app req respond =
    case requestMethod req of
      "CONNECT" ->
        respond (responseRaw connProxy (responseLBS methodNotAllowed405 [] ""))
      _ -> app req respond
  where
    {- |
      Open a connection to the local ssh port and mediate the traffic between
      that service and the client.
    -}
    connProxy :: IO ByteString -> (ByteString -> IO ()) -> IO ()
    connProxy read_ write =
      bracket
        (socket AF_INET Stream defaultProtocol)
        (\so ->  close so `finally` write "") 
        (\so -> do
          connect so =<<
            (
              getAddrInfo Nothing (Just "127.0.0.1") (Just "22") >>= \case
                [] -> fail "Address not found: 127.0.0.1:22"
                sa:_ -> return (addrAddress sa)
            )
          concurrently_
            (pipeInbound so read_)
            (pipeOutbound so write)
        )

    {- | Forward data coming from the client, going to the ssh service. -}
    pipeInbound :: Socket -> IO ByteString -> IO ()
    pipeInbound so read_ = do
      bytes <- read_
      if BS.null bytes
        then return ()
        else do
          sendAll so bytes
          pipeInbound so read_

    {- | Forward data coming from the ssh service, going to the client. -}
    pipeOutbound :: Socket -> (ByteString -> IO ()) -> IO ()
    pipeOutbound so write = do
      bytes <- recv so 4096
      write bytes
      if BS.null bytes
        then return ()
        else pipeOutbound so write


{- | Serve a static page at the given 'pathInfo'. -}
staticPage
  :: [Text] {- ^ The path info. -}
  -> ByteString {- ^ The content type. -}
  -> BSL.ByteString {- ^ The response body content. -}
  -> Middleware
staticPage path ct bytes app req respond =
  if pathInfo req == path
    then respond (responseLBS ok200 [("Content-Type", ct)] bytes)
    else app req respond


{- | Rewrite: "\/" -> "/index.html". -}
defaultIndex :: Middleware
defaultIndex app request respond =
  case pathInfo request of
    [] -> app request {pathInfo = ["index.html"]} respond
    _ -> app request respond


{- |
  A bearer token, which is an instance of the necessary type classes to
  be useful as a servant header value.
-}
newtype BearerToken = BearerToken {
    unBearerToken :: Text
  }
instance ToHttpApiData BearerToken where
  toUrlPiece t = "Bearer " <> unBearerToken t


{- |
  The Template-Haskell splice @$$(staticSite dir)@ will build a
  'Middleware' that serves a set of static files determined at
  compile time, or else passes the request to the underlying
  'Network.Wai.Application'.

  All files under @dir@ will be served relative to the root path of
  your web server, so the file @\<dir\>\/foo\/bar.html@ will be served at
  @http://your-web-site.com/foo/bar.html@

  The content-type of the files being served will be guessed using
  'defaultMimeLookup'.
-}
staticSite :: FilePath -> Q (TExp Middleware)
staticSite baseDir = join . runIO $ do
    files <- readStaticFiles
    mapM_ (printResource . fst) files
    return $ mapM_ (addDependentFile . ((baseDir ++ "/") ++) . fst) files >> [||
        let
          {- |
            Build a middleware that serves a single static file path, or
            delegates to the underlying application.
          -}
          static :: (FilePath, String) -> Middleware
          static (filename, content) app req respond =
            let
              {- | Guess the content type of the static file. -}
              ct :: ByteString
              ct =
                defaultMimeLookup
                . fromString
                $ filename
            in
              if pathInfo req == T.split (== '/') (T.pack filename)
                then
                  respond (
                      responseLBS
                        ok200
                        [("content-type", ct)]
                        (BSL8.pack content)
                    )
                else app req respond
        in
          foldr (.) id (fmap static files) :: Middleware
      ||]
  where
    printResource :: String -> IO ()
    printResource file =
      putStrLn ("Generating static resource for: " ++ show file)

    {- | Reads the static files that make up the admin user interface. -}
    readStaticFiles :: IO [(FilePath, String)]
    readStaticFiles =
      let
        findAll :: FilePath -> IO [FilePath]
        findAll dir = do
            contents <-
              (\\ [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
            dirs <- catMaybes <$> mapM justDir contents
            files <- catMaybes <$> mapM justFile contents
            more <- concat <$> mapM (findAll . combine dir) dirs
            return $ (combine dir <$> files) ++ more
          where
            justFile :: FilePath -> IO (Maybe FilePath)
            justFile filename = do
              isfile <-
                isRegularFile <$>
                  getFileStatus (baseDir </> dir </> filename)
              return $ if isfile then Just filename else Nothing

            justDir :: FilePath -> IO (Maybe FilePath)
            justDir filename = do
              isdir <-
                isDirectory <$>
                  getFileStatus (baseDir </> dir </> filename)
              return $ if isdir then Just filename else Nothing
      in do
        allFiles <- findAll "."
        allContent
          <- mapM (fmap BS8.unpack . BS.readFile . combine baseDir) allFiles
        return (zip (drop 2 <$> allFiles) allContent)


{- |
  Read and render a markdown file at compile time.

  The Template-Haskell splice @$$(staticMarkdown "docs/api.md")@ will
  produce a Haskell string-like value that contains the markdown in
  "docs/api.md" rendered as HTML. You can use 'staticPage' when you want
  to actually serve the rendered markdown.
-}
staticMarkdown :: (IsString a) => FilePath -> Q (TExp a)
staticMarkdown file = do
    addDependentFile file
    fmap
      (
        TExp
        . AppE (VarE 'fromString)
        . LitE
        . StringL
        . renderMarkup
        . render
        . (:[])
        . commonmarkToNode mempty mempty
      )
      (runIO (TIO.readFile file))
  where
    {- | Render some random markdown, with table of contents. -}
    render :: [Node] -> Html
    render nodes =
      div ! doc $ do
        renderToc nodes
        renderBody nodes


    {- | Render the body of some random markdown. -}
    renderBody :: [Node] -> Html

    renderBody [] = mempty

    renderBody (Node _ DOCUMENT docNodes:remaining) = do
      div ! docBody $ renderBody docNodes
      renderBody remaining

    renderBody (Node _ (HEADING level) hnodes:nodes) =
        let
          sectionNodes :: [Node]
          remaining :: [Node]

          (sectionNodes, remaining) = break (isNewSection level) nodes
        in
          renderSection level hnodes sectionNodes
          <> renderBody remaining
      where
        {- | Render an individual documentation section or subsection. -}
        renderSection :: Level -> [Node] -> [Node] -> Html
        renderSection _level heading section =
          div ! docSection $ do
            div ! docSectionHeading $
              a ! HTML.id (hash heading) $
                renderBody heading
            div ! docSectionBody $ renderBody section


    renderBody (Node _ THEMATIC_BREAK children:remaining) =
      hr <> renderBody children <> renderBody remaining

    renderBody (Node _ PARAGRAPH children:remaining) = do
      p $
        renderBody children
      renderBody remaining

    renderBody (Node _ BLOCK_QUOTE children:remaining) =
      undefined children remaining

    renderBody (Node _ (HTML_BLOCK txt) children:remaining) =
      undefined txt children remaining

    renderBody (Node _ (CUSTOM_BLOCK _ _) children:remaining) =
      undefined children remaining

    renderBody (Node _ (CODE_BLOCK _info txt) children:remaining) = do
      pre $
        code $
          text txt
      renderBody children
      renderBody remaining

    renderBody (Node _ (LIST attrs) children:remaining) =
      let
        tag :: Html -> Html
        tag =
          case listType attrs of
            BULLET_LIST -> ul
            ORDERED_LIST -> ol
      in do
        tag $ renderBody children
        renderBody remaining

    renderBody (Node _ ITEM children:remaining) = do
      li $ renderBody children
      renderBody remaining

    renderBody (Node _ (TEXT txt) children:remaining) = do
      text txt
      renderBody children
      renderBody remaining

    renderBody (Node _ SOFTBREAK children:remaining) =
      " " <> renderBody children <> renderBody remaining

    renderBody (Node _ LINEBREAK children:remaining) = do
      br
      renderBody children
      renderBody remaining

    renderBody (Node _ (HTML_INLINE txt) children:remaining) =
      undefined children remaining txt

    renderBody (Node _ (CUSTOM_INLINE onenter onexit) children:remaining) =
      undefined children remaining onenter onexit

    renderBody (Node _ (CODE txt) children:remaining) = do
      code $
        text txt
      renderBody children
      renderBody remaining

    renderBody (Node _ EMPH children:remaining) = do
      em $ renderBody children
      renderBody remaining

    renderBody (Node _ STRONG children:remaining) = do
      strong $ renderBody children
      renderBody remaining

    renderBody (Node _ (LINK url _title) children:remaining) = do
      a ! src (toValue url) $ renderBody children
      renderBody remaining

    renderBody (Node _ (IMAGE url _title) children:remaining) = do
      img ! src (toValue url)
      renderBody children
      renderBody remaining

    renderBody (Node _ STRIKETHROUGH children:remaining) =
      undefined children remaining

    renderBody (Node _ (TABLE alignments) children:remaining) =
      undefined children remaining alignments

    renderBody (Node _ TABLE_ROW children:remaining) =
      undefined children remaining

    renderBody (Node _ TABLE_CELL children:remaining) =
      undefined children remaining


    {- | Documentation class. -}
    doc :: Attribute
    doc = class_ "documentation"


    {- | Documentation TOC class. -}
    docToc :: Attribute
    docToc = class_ "documentation-toc"


    {- | Documentation body class. -}
    docBody :: Attribute
    docBody = class_ "documentation-body"


    {- | Documentation section class. -}
    docSection :: Attribute
    docSection = class_ "documentation-section"


    {- | Documentation section heading class. -}
    docSectionHeading :: Attribute
    docSectionHeading = class_ "documentation-section-heading"


    {- | Documentation section body class. -}
    docSectionBody :: Attribute
    docSectionBody = class_ "documentation-section-body"


    {- | Create an Id for the node. -}
    hash :: [Node] -> AttributeValue
    hash (Node (Just (PosInfo l m n o)) _ _:_) =
      fromString
      . intercalate "-"
      . fmap show
      $ [l, m, n, o]
    hash _ = "undefined"


    {- | Render a table of contents. -}
    renderToc :: [Node] -> Html
    renderToc [] = mempty
    renderToc (Node _ DOCUMENT nodes:remaining) = do
      div ! docToc $ do
        div ! class_ "toc-header" $ text "Table of Contents"
        renderToc nodes
      renderToc remaining
    renderToc (Node _ THEMATIC_BREAK nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ PARAGRAPH nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ BLOCK_QUOTE nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ ITEM nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ SOFTBREAK nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ LINEBREAK nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ EMPH nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ STRONG nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ STRIKETHROUGH nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ TABLE_ROW nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ TABLE_CELL nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (HTML_BLOCK _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (CUSTOM_BLOCK _ _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (CODE_BLOCK _ _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (HEADING level) nodes:remaining) =
        let
          followingSections :: [Node]
          (sectionNodes, followingSections) = break (isNewSection level) remaining
        in do
          div $ do
            renderTocEntry nodes
            renderToc sectionNodes
          renderToc followingSections
      where
        renderTocEntry :: [Node] -> Html
        renderTocEntry tocNodes =
          a ! href ("#" <> hash tocNodes) $ text (foldMap getText tocNodes)

    renderToc (Node _ (LIST _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (TEXT _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (HTML_INLINE _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (CUSTOM_INLINE _ _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (CODE _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (LINK _ _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (IMAGE _ _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining
    renderToc (Node _ (TABLE _) nodes:remaining) = do
      renderToc nodes
      renderToc remaining


    {- | Figure out if the node represents the beginning of a new section. -}
    isNewSection :: Level -> Node -> Bool
    isNewSection level (Node _ (HEADING l) _) | l <= level = True
    isNewSection _level _ = False


    {- | Get the text value of a node. -}
    getText :: Node -> Text
    getText (Node _ DOCUMENT nodes) = foldMap getText nodes
    getText (Node _ THEMATIC_BREAK nodes) = foldMap getText nodes
    getText (Node _ PARAGRAPH nodes) = foldMap getText nodes
    getText (Node _ BLOCK_QUOTE nodes) = foldMap getText nodes
    getText (Node _ (HTML_BLOCK _) nodes) = foldMap getText nodes
    getText (Node _ (CUSTOM_BLOCK _ _) nodes) = foldMap getText nodes
    getText (Node _ (CODE_BLOCK _ _) nodes) = foldMap getText nodes
    getText (Node _ (HEADING _) nodes) = foldMap getText nodes
    getText (Node _ (LIST _) nodes) = foldMap getText nodes
    getText (Node _ ITEM nodes) = foldMap getText nodes
    getText (Node _ (TEXT txt) nodes) = txt <> foldMap getText nodes
    getText (Node _ SOFTBREAK nodes) = foldMap getText nodes
    getText (Node _ LINEBREAK nodes) = foldMap getText nodes
    getText (Node _ (HTML_INLINE _) nodes) = foldMap getText nodes
    getText (Node _ (CUSTOM_INLINE _ _) nodes) = foldMap getText nodes
    getText (Node _ (CODE _) nodes) = foldMap getText nodes
    getText (Node _ EMPH nodes) = foldMap getText nodes
    getText (Node _ STRONG nodes) = foldMap getText nodes
    getText (Node _ (LINK _ _) nodes) = foldMap getText nodes
    getText (Node _ (IMAGE _ _) nodes) = foldMap getText nodes
    getText (Node _ STRIKETHROUGH nodes) = foldMap getText nodes
    getText (Node _ (TABLE _) nodes) = foldMap getText nodes
    getText (Node _ TABLE_ROW nodes) = foldMap getText nodes
    getText (Node _ TABLE_CELL nodes) = foldMap getText nodes


