{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude                    (catchAny)
import Control.Applicative              (optional)
import Control.Exception                (catch, IOException)
import Control.Monad                    (liftM, liftM2, join)
import Data.Maybe                       (catMaybes, isJust)
import qualified Data.ByteString.Char8 as C8           (append, cons, pack, unpack)
import System.Directory                 (removeFile, getTemporaryDirectory)
import System.IO                        (hClose)
import System.IO.Temp                   (openTempFile)
import System.Posix.ByteString.FilePath (RawFilePath, withFilePath)
import System.Posix.Files.ByteString    (fileExist, createNamedPipe)
import System.Posix.IO.ByteString       (fdWrite, openFd, closeFd, fdWrite, OpenMode(..), defaultFileFlags)
import System.Posix.Types               (ByteCount, Fd, FileMode)
import System.Exit                      ( ExitCode(..) )
import System.Process                   (readProcessWithExitCode)
import qualified Data.Text as T

data Compiler = Compiler {command :: FilePath,
                          inputOption :: [Pipe] -> String,
                          outputOption :: RawFilePath -> String,
                          otherOptions :: [String],
                          isSucessful :: (ExitCode, String, String) -> Bool}

-- | Wrapper type for `Source` `Text`
newtype Source = Source T.Text

-- | Wrapper type to distinguish pipes from other files
newtype Pipe = Pipe RawFilePath

-- | Remove a file, if it exists (to prevent uncaught exceptions)
removeIfExists :: FilePath -> IO ()
removeIfExists filePath = do
  catch (removeFile filePath) $ \(e :: IOException) -> do
    return ()

-- | Catch most, if not all, exceptions and return `Nothing`, in `IO`, upon failure
nothingIfException :: IO a -> IO (Maybe a)
nothingIfException f = catchAny (optional f) $ \e -> do
  return Nothing

-- | If the input returns an `IOError`, return `Nothing`, in IO
nothingIfIOError :: IO a -> IO (Maybe a)
nothingIfIOError io = liftM (join . Just) . catch (optional io) $ \(e :: IOError) -> do
  return Nothing


-- | Warning, will loop if can't find/make temp file
makeTempFileName :: RawFilePath -> String -> IO RawFilePath
makeTempFileName dir name = do
  (filePath, handle) <- openTempFile (show dir) name
  nothingIfException $ hClose handle
  removeIfExists filePath
  exists <- fileExist . C8.append dir . C8.pack $ '/' : name
  if exists
  then
    do
      makeTempFileName dir name
  else
    do
      return (C8.pack filePath)

-- | Given a directory, filename template, and filemode, create a named pipe
-- or return `Nothing`, in `IO`
createTempPipe :: RawFilePath -> String -> FileMode -> IO (Maybe Pipe)
createTempPipe dir name mode = do
  filePath <- makeTempFileName dir name
  result <- nothingIfException $ createNamedPipe filePath mode
  return $ result >>= const (Just . Pipe $ filePath)

-- | Open a `Pipe`, returning `Nothing`, in `IO`, upon failure
openPipe :: Pipe -> IO (Maybe Fd)
openPipe (Pipe filePath) = nothingIfIOError $ openFd filePath ReadOnly Nothing defaultFileFlags

-- | See `removeIfExists`
removePipeIfExists :: Pipe -> IO ()
removePipeIfExists (Pipe pipeName) = removeIfExists . C8.unpack $ pipeName

-- | The type gives it away: it's like `liftM`, but pushes the `Maybe` inside the `IO`
liftMaybeIO :: (a -> IO (Maybe b)) -> Maybe a -> IO (Maybe b)
liftMaybeIO f Nothing  = return Nothing
liftMaybeIO f (Just x) = f x

-- | `fdWrite`, but allowing for a `Nothing` `Fd` and write failure
safeWrite :: Maybe Fd -> String -> IO (Maybe ByteCount)
safeWrite fd str = liftMaybeIO (nothingIfException . flip fdWrite str) fd

-- | Returns `Just True` on complete success, `Just False` if not everything
-- was able to be written, or `Nothing` upon exception
checkedWrite :: Maybe Fd -> String -> IO (Maybe Bool)
checkedWrite fd str = do
  let shouldBeWritten = Just . length $ str
  maybeWritten <- (fmap . fmap) fromEnum $ safeWrite fd str
  return $ liftM2 (==) shouldBeWritten maybeWritten

-- | This function writes a `Source` to a `Pipe`, returning just the `Pipe` on
-- success, else `Nothing`
writeSourceToPipe :: Source -> Pipe -> IO (Maybe Pipe)
writeSourceToPipe (Source source) pipe = do
  let sourceString = T.unpack source
  let bytesShouldBeWritten = length sourceString
  pipeFd <- openPipe pipe
  didWrite <- checkedWrite pipeFd sourceString
  liftMaybeIO (nothingIfException . closeFd) pipeFd
  if isJust didWrite
  then
    do
      return (Just pipe)
  else
    do
      removePipeIfExists pipe
      return Nothing

-- | Make a named pipe, write the source to it, and return `Just` `Pipe` on success
sourceToPipe :: Source -> IO (Maybe Pipe)
sourceToPipe source = do
  pipe <- getTemporaryDirectory >>= flip (flip createTempPipe "") 644 . C8.pack
  liftMaybeIO (writeSourceToPipe source) pipe

-- | Run a `Compiler` on a list of `Source`s
runCompiler :: Compiler -> [Source] -> IO (Maybe RawFilePath)
runCompiler compiler sources = do
  sourcePipes <- mapM sourceToPipe sources
  tempDir <- getTemporaryDirectory
  outputFile <- makeTempFileName (C8.pack tempDir) "compiled"
  let options = [inputOption compiler . catMaybes $ sourcePipes, outputOption compiler outputFile, unwords $ otherOptions compiler]
  result <- readProcessWithExitCode (command compiler) options ""
  if isSucessful compiler result
  then
    do
      mapM_ removePipeIfExists $ catMaybes sourcePipes
      return (Just outputFile)
  else
    do
      mapM_ removePipeIfExists $ catMaybes sourcePipes
      removeIfExists . C8.unpack $ outputFile
      return Nothing

