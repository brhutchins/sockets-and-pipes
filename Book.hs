{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Book where

import Prelude ()
import Relude

import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO

import Control.Monad.Trans.Resource
  (ReleaseKey, ResourceT, allocate, runResourceT)

import qualified Control.Exception.Safe as Ex

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Char as Char

helloText :: IO ()
helloText = T.hPutStrLn stdout (T.pack "hello world!")

helloTextFile :: IO ()
helloTextFile = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") WriteMode
  liftIO do
    T.hPutStrLn h (T.pack "hello")
    T.hPutStrLn h (T.pack "world")

getDataDir :: IO FilePath
getDataDir = do
  dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
  Dir.createDirectoryIfMissing True dir
  pure dir

writeGreetingSafe :: IO ()
writeGreetingSafe = runResourceT @IO do
  dir <- liftIO getDataDir
  (_releaseKey, h) <- fileResource (dir </> "greeting.txt") WriteMode
  liftIO (IO.hPutStrLn h "hello")
  liftIO (IO.hPutStrLn h "world")

fileResource ::
  FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode =
  allocate (IO.openFile path mode)
            IO.hClose

handlePrintTest :: IO ()
handlePrintTest = runResourceT @IO do
  dir <- liftIO getDataDir
  (_releaseKey, h) <- fileResource(dir </> "greeting.txt") WriteMode
  liftIO (IO.print h)
  str <- liftIO $ IO.hShow h
  liftIO (IO.print  str)
  (_releaseKey', h') <- fileResource "/home/barnaby/test.db" ReadMode
  liftIO $ IO.print h'
  str' <- liftIO $ IO.hShow h'
  liftIO $ IO.print str'

howManyHandles :: IO ()
howManyHandles = runResourceT @IO do
  hs <- openManyHandles
  putStrLn ("Opened " <> show (length hs) <> " handles")

openManyHandles :: ResourceT IO [Handle]
openManyHandles = do
    h <- fileResourceMaybe
    case h of
      Nothing -> pure []
      Just h' -> do
        hs <- openManyHandles
        pure $ h' : hs

fileResourceMaybe :: ResourceT IO (Maybe Handle)
fileResourceMaybe = do
  dir <- liftIO getDataDir
  result <- Ex.tryIO do
    (_, h) <- allocate (IO.openFile (dir </> "greeting.txt") ReadMode) IO.hClose
    pure h
  case result of
    Right x -> pure $ Just x
    Left e -> do
      print $ displayException e
      pure Nothing

printFileContentsUpperCase :: IO ()
printFileContentsUpperCase = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
  liftIO $ printCapitalizedText h

printCapitalizedText :: Handle -> IO ()
printCapitalizedText h = continue
  where
    continue = do
      chunk <- T.hGetChunk h
      case T.null chunk of
        True -> pure ()
        False -> do
          T.putStr (T.toUpper chunk)
          continue

repeatUntilIO :: IO chunk -- ^ Producer of chunks
  -> (chunk -> Bool) -- ^ Does chunk indicate end of file?
  -> (chunk -> IO x) -- ^ What to do with each chunk
  -> IO ()
repeatUntilIO getChunk isEnd f = continue
  where
    continue = do
      chunk <- getChunk
      case isEnd chunk of
        True -> pure ()
        False -> do { _ <- f chunk; continue }

printFileContentsUpperCase2 :: IO ()
printFileContentsUpperCase2 = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
  liftIO $ repeatUntilIO (T.hGetChunk h) T.null \chunk ->
    T.putStr (T.toUpper chunk)

digitsOnly :: Text -> Text
digitsOnly = T.filter Char.isDigit

capitalizeLast :: Text -> Text
-- capitalizeLast txt = T.reverse $ T.singleton (Char.toUpper $ T.head $ T.reverse txt) <> T.tail (T.reverse txt)
capitalizeLast txt = T.init txt <> T.singleton (Char.toUpper $ T.last txt)

unParen :: Text -> Maybe Text
unParen txt = case
  T.length (T.filter (== '(') txt) > 0 &&
  T.length (T.filter (== ')') txt) > 0
  of
    True -> Just $ T.replace (T.singleton ')') (T.pack "") $
                   T.replace (T.singleton '(') (T.pack "") txt
    False -> Nothing

characterCountUnChunked :: FilePath -> IO Int
characterCountUnChunked fp = do
  dir <- getDataDir
  x <- T.readFile (dir </> fp)
  pure $ T.length x

characterCount :: FilePath -> IO Int
characterCount fp = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> fp) ReadMode
  liftIO $ countByChunk h
    where
      countByChunk :: Handle -> IO Int
      countByChunk h = do
        chunk <- T.hGetChunk h
        case T.null chunk of
          True -> pure 0
          False -> do
            nextChunkLength <- countByChunk h
            pure $ T.length chunk + nextChunkLength
