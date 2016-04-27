
import Control.Exception                (catch)
import Control.Monad                    (liftM)
import qualified Data.ByteString.Char8 as C8           (pack, unpack)
import System.Directory                 (removeFile)
import System.IO                        (hClose)
import System.IO.Temp                   (openTempFile)
import System.Posix.ByteString.FilePath (RawFilePath, withFilePath)
import System.Posix.Files.ByteString    (fileExist)
import System.Posix.IO.ByteString       (fdWrite, openFd, closeFd, fdWrite, OpenMode(..), defaultFileFlags)
import System.Posix.Types               (Fd)
import System.Exit                      ( ExitCode(..) )
import System.Process                   (readProcessWithExitCode)
import qualified Data.Text as T
-- System.Posix.Files.ByteString (createNamedPipe,
-- Ensure pipe can be made
-- Make pipe for source
-- Ensure pipe made and is writable
-- Pipe source into sourcePipe
-- make temp file
-- Call compiler on sourcePipe to temp file
-- Ensure finished without error
-- Close source pipe
-- Call dlopen on temp file
-- Call dlsym on handle


data Compiler = Compiler {command :: FilePath,
                          inputOption :: [Pipe] -> String,
                          outputOption :: OutputFilePath -> String,
                          otherOptions :: [String],
                          isSucessful :: (ExitCode, String, String) -> Bool}

newtype Source = Source T.Text
newtype Pipe = Pipe RawFilePath
newtype OutputFilePath = OutputFilePath RawFilePath

makeTemporaryPipe :: IO (Maybe Pipe)
makeTemporaryPipe = undefined

openPipe :: Pipe -> IO Fd
openPipe (Pipe filePath) = openFd filePath ReadOnly Nothing defaultFileFlags

rawToFilePath :: RawFilePath -> FilePath
rawToFilePath = show

-- | This function writes a `Source` to a `Pipe`, returning just the `Pipe` on
-- success, else `Nothing`
writeSourceToPipe :: Source -> Pipe -> IO (Maybe Pipe)
writeSourceToPipe (Source source) pipe = do
  pipeFd <- openPipe pipe
  let sourceString = T.unpack source
  bytesWritten <- fdWrite pipeFd sourceString
  let bytesShouldBeWritten = length sourceString
  closeFd pipeFd
  if (toEnum bytesWritten) == bytesShouldBeWritten
  then
    do
      return (Just pipe)
  else
    do
      let Pipe pipeName = pipe
      removeIfExists (rawToFilePath pipeName)
      return Nothing


sourceToPipe :: Source -> IO (Maybe Pipe)
sourceToPipe (Source source) = do
  pipe <- makeTemporaryPipe
  output <- liftM (writeSourceToPipe source) pipe
  output

-- isGoodPipe :: Pipe -> IO Bool

-- runCompiler compiler sources = do
--   sourcePipes <- mapM sourceToPipe sources
--   outputFile <- makeSystemTempFile
--   let options = [inputOption sourcePipes, outputOption outputFile, otherOptions]
--   result <- readProcessWithExitCode (command compiler) options ""
--   if isSucessful compiler result
--   then
--     do
--       mapM_ removeIfExists sourcePipes
--       return (Just outputFile)
--   else
--     do
--       mapM_ removeIfExists sourcePipes
--       removeIfExists outputFile
--       return Nothing

nothingIfMinus1 -1 = Nothing
nothingIfMinus1  x = Just x

rawFilePathToNonRaw :: RawFilePath -> FilePath
rawFilePathToNonRaw = C8.unpack

removeIfExists :: FilePath -> IO ()
removeIfExists filePath = do
  Control.Exception.catch (removeFile filePath) $ \e -> do
    return ()

nothingIfException :: IO a -> IO (Maybe a)
nothingIfException f = do
  result <- Control.Exception.catch f $ \e -> do
    return Nothing
  return (Just result)

makeTempFileName :: RawFilePath -> String -> IO RawFilePath
makeTempFileName dir name = do
  (filePath, handle) <- openTempFile dir name
  hClose handle
  removeIfExists filePath
  exists <- fileExist $ dir ++ name
  if exists
  then
    do
      makeTempFileName dir name
  else
    do
      return (C8.pack filePath)



-- createTempFifo dir name mode = do
--   filePath <- makeTempFileName dir name
--   withFilePath filePath $ \s ->
--     nothingIfMinus1 (undefined s mode) -- prev: c_mkfifo



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- -- System-Posix-Files-ByteString
-- -- | @createNamedPipe fifo mode@
-- -- creates a new named pipe, @fifo@, with permissions based on
-- -- @mode@. May fail with 'throwErrnoPathIfMinus1_' if a file named @name@
-- -- already exists or if the effective user ID of the current process doesn't
-- -- have permission to create the pipe.
-- --
-- -- Note: calls @mkfifo@.
-- createNamedPipe :: RawFilePath -> FileMode -> IO ()
-- createNamedPipe name mode = do
--   System.Posix.ByteString.FilePath.withFilePath name $ \s ->
--     throwErrnoPathIfMinus1_ "createNamedPipe" name (c_mkfifo s mode)

-- withFilePath :: RawFilePath -> (CString -> IO a) -> IO a

-----------------------------------------------------------------------------------------------------
-- import Prelude hiding (catch)
-- import System.Directory
-- import Control.Exception
-- import System.IO.Error hiding (catch)

-- removeIfExists :: FilePath -> IO ()
-- removeIfExists fileName = removeFile fileName `catch` handleExists
--   where handleExists e
--           | isDoesNotExistError e = return ()
--           | otherwise = throwIO e
-----------------------------------------------------------------------------------------------------



-- Data.ByteString.Char8.useAsCString :: ByteString -> (CString -> IO a) -> IO a Source
-- O(n) construction Use a ByteString with a function requiring a null-terminated CString. The CString is a copy and will be freed automatically.



-- -- Don't forget to allow for multiple source files, so have some sort of (Text -> IO RawFilePath) function, which is mapped over [Text], i.e. [SourceFile]
-- runCompiler :: Compiler -> Text -> IO (Maybe Ptr ())
-- runCompiler compiler source = do
--   sourcePipe <- makeTempPipe
--   sourcePipeIsGood <- isGoodPipe sourcePipe
--   sourceFd <- openFd sourcePipe WriteOnly (Just _) defaultFileFlags
--   allWritten <- writeTextToFd source sourceFd

--   tempFilename <- makeTempFile

--   results <- runCommand (command compiler) (inputOption compiler sourcePipe) (outputOption compiler tempFilename) (otherOptions compiler)

--   closedSource <- closePipe sourcePipe

--   finalPtr <- c_dlopen (toCString tempFilename) (dlopenOptions)
--   if finalPtr == nullPtr
--      then return Nothing
--      else return (Just finalPtr)







-- runCompiler :: Compiler          -- The compiler spec
--             -> RawFilePath       -- The input named-pipe
--             -> RawFilePath       -- The output filename
--             -> IO (Maybe T.Text) -- The error messages
-- runCompiler compiler inName outName = do
--   let fullCommand = T.unwords [command, inputOption inName, outputOption outName, otherOptions]
--   let cmdSpec = ShellCommand . show $ fullCommand
--   let process = CreateProcess  {cmdspec             = cmdSpec,
--                                 cwd                 = Nothing,
--                                 env                 = Nothing,
--                                 std_in              = NoStream,
--                                 std_out             = CreatePipe,
--                                 std_err             = CreatePipe,
--                                 close_fds           = True,
--                                 create_group        = False,
--                                 delegate_ctlc       = False,
--                                 detach_console      = False,
--                                 create_new_console  = False,
--                                 new_session         = False,
--                                 child_group         = Nothing,
--                                 child_user          = Nothing}
--   (_, stdoutPipe, stderrPipe, pHandle) <- createProcess process
--   exitCode <- waitForProcess pHandle
--   stdoutEmpty <- liftM hIsEOF stdoutPipe
--   stderrEmpty <- liftM hIsEOF stderrPipe
--   if (exitCode == ExitSuccess) && stdoutEmpty && stderrEmpty
--   then
--     do
--       return Nothing
--   else
--     do
--       if debug then liftM putStrLn . hGetContents $ stdoutPipe >> liftM putStrLn . hGetContents $ stderrPipe
--       return Just (T.pack "Compilation was not sucessful")


-- System.Posix.Temp.ByteString
-- | Make a unique filename and open it for reading\/writing. The returned
-- 'RawFilePath' is the (possibly relative) path of the created file, which is
-- padded with 6 random characters. The argument is the desired prefix of the
-- filepath of the temporary file to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
-- mkstemp :: ByteString -> IO (RawFilePath, Handle)
-- mkstemp template' = do
--   let template = template' `B.append` (BC.pack "XXXXXX")
--   withFilePath template $ \ ptr -> do
--     fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
--     name <- peekFilePath ptr
--     h <- fdToHandle (Fd fd)
--     return (name, h)




-- -- System.Posix.ByteString.FilePath

-- -- | as 'throwErrno', but exceptions include the given path when appropriate.
-- --
-- throwErrnoPath :: String -> RawFilePath -> IO a
-- throwErrnoPath loc path =
--   do
--     errno <- getErrno
--     ioError (errnoToIOError loc errno Nothing (Just (BC.unpack path)))

-- -- | as 'throwErrnoIf', but exceptions include the given path when
-- --   appropriate.
-- --
-- throwErrnoPathIf :: (a -> Bool) -> String -> RawFilePath -> IO a -> IO a
-- throwErrnoPathIf cond loc path f =
--   do
--     res <- f
--     if cond res then throwErrnoPath loc path else return res

-- -- | as 'throwErrnoIf_', but exceptions include the given path when
-- --   appropriate.
-- --
-- throwErrnoPathIf_ :: (a -> Bool) -> String -> RawFilePath -> IO a -> IO ()
-- throwErrnoPathIf_ cond loc path f  = void $ throwErrnoPathIf cond loc path f


-- -- | as 'throwErrnoIfMinus1', but exceptions include the given path when
-- --   appropriate.
-- --
-- throwErrnoPathIfMinus1 :: (Eq a, Num a) => String -> RawFilePath -> IO a -> IO a
-- throwErrnoPathIfMinus1 = throwErrnoPathIf (== -1)

-- -- | as 'throwErrnoIfMinus1_', but exceptions include the given path when
-- --   appropriate.
-- --
-- throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> RawFilePath -> IO a -> IO ()
-- throwErrnoPathIfMinus1_  = throwErrnoPathIf_ (== -1)
































