{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
  Module containing the 'staticSite' function. It exists separately from
  'OM.HTTP' solely for the purpose of having hlint avoid analyzing the
  source code contained herein. HLint does not support typed template
  haskell quotations and will crash when it encounters them, but we
  still want to run HLint on everything else.
-}
module OM.HTTP.StaticSite (
  staticSite,
) where


import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.List ((\\))
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Language.Haskell.TH (TExp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import Network.HTTP.Types (ok200)
import Network.Mime (defaultMimeLookup)
import Network.Wai (Middleware, responseLBS, pathInfo)
import System.Directory (getDirectoryContents)
import System.FilePath.Posix (combine, (</>))
import System.Posix.Files (isRegularFile, isDirectory, getFileStatus)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T


{- |
  The Template-Haskell splice @$$(staticSite dir)@ will build a
  'Middleware' that serves a set of static files determined at
  compile time, or else passes the request to the underlying
  'Network.Wai.Application'.

  All files under @dir@ will be served relative to the root path of
  your web server, so the file @\<dir\>\/foo\/bar.html@ will be served at
  @http://your-web-site.com/foo/bar.html@
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
              contentType :: ByteString
              contentType =
                defaultMimeLookup
                . fromString
                $ filename
            in
              if pathInfo req == T.split (== '/') (T.pack filename)
                then
                  respond (
                      responseLBS
                        ok200
                        [("content-type", contentType)]
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


