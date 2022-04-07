{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module IOUtils (
  IOUtils.interact,
  IOUtils.putChar, IOUtils.putStr, IOUtils.putStrLn, IOUtils.print,
  IOUtils.getChar, IOUtils.getLine, IOUtils.getContents, IOUtils.readIO,
  IOUtils.readLn,
  ePutChar, ePutStr, ePutStrLn, ePrint,
  trace, traceIO
  ) where

#ifdef mingw32_HOST_OS

import System.Win32.Types (BOOL, HANDLE, DWORD, LPDWORD, LPWSTR, LPCWSTR, LPVOID)
import Foreign.C.Types (CWchar)
import Foreign
import Prelude hiding (getContents, putStr, putStrLn) --(IO, Read, Show, String)
--import qualified System.IO
import qualified System.IO (getContents)
import System.IO hiding (getContents, putStr, putStrLn)
import Data.Char (ord)
import System.IO.Unsafe(unsafePerformIO)

 {- <http://msdn.microsoft.com/en-us/library/ms683231(VS.85).aspx>
    HANDLE WINAPI GetStdHandle(DWORD nStdHandle);
    returns INVALID_HANDLE_VALUE, NULL, or a valid handle -}

foreign import ccall unsafe "GetStdHandle" win32GetStdHandle :: DWORD -> IO (HANDLE)

std_OUTPUT_HANDLE = -11 :: DWORD  -- all DWORD arithmetic is performed modulo 2^n
std_ERROR_HANDLE  = -12 :: DWORD

 {- <http://msdn.microsoft.com/en-us/library/aa364960(VS.85).aspx>
    DWORD WINAPI GetFileType(HANDLE hFile); -}

foreign import ccall unsafe "GetFileType" win32GetFileType :: HANDLE -> IO (DWORD)
_FILE_TYPE_CHAR   = 0x0002 :: DWORD
_FILE_TYPE_REMOTE = 0x8000 :: DWORD

 {- <http://msdn.microsoft.com/en-us/library/ms683167(VS.85).aspx>
    BOOL WINAPI GetConsoleMode(HANDLE hConsole, LPDWORD lpMode); -}

foreign import ccall unsafe "GetConsoleMode" win32GetConsoleMode :: HANDLE -> LPDWORD -> IO (BOOL)
_INVALID_HANDLE_VALUE = (intPtrToPtr $ -1) :: HANDLE

is_a_console :: HANDLE -> IO (Bool)
is_a_console handle
  = if (handle == _INVALID_HANDLE_VALUE) then return False
      else do ft <- win32GetFileType handle
              if ((ft .&. complement _FILE_TYPE_REMOTE) /= _FILE_TYPE_CHAR) then return False
                else do ptr <- malloc
                        cm  <- win32GetConsoleMode handle ptr
                        free ptr
                        return cm

real_stdout :: IO (Bool)
real_stdout = is_a_console =<< win32GetStdHandle std_OUTPUT_HANDLE

real_stderr :: IO (Bool)
real_stderr = is_a_console =<< win32GetStdHandle std_ERROR_HANDLE

 {- BOOL WINAPI WriteConsoleW(HANDLE hOutput, LPWSTR lpBuffer, DWORD nChars,
                              LPDWORD lpCharsWritten, LPVOID lpReserved); -}

foreign import ccall unsafe "WriteConsoleW" win32WriteConsoleW
  :: HANDLE -> LPWSTR -> DWORD -> LPDWORD -> LPVOID -> IO (BOOL)

data ConsoleInfo = ConsoleInfo Int (Ptr CWchar) (Ptr DWORD) HANDLE

writeConsole :: ConsoleInfo -> [Char] -> IO ()
writeConsole (ConsoleInfo bufsize buf written handle) string
  = let fillbuf :: Int -> [Char] -> IO ()
        fillbuf i [] = emptybuf buf i []
        fillbuf i remain@(first:rest)
          | i + 1 < bufsize && ordf <= 0xffff = do pokeElemOff buf i asWord
                                                   fillbuf (i+1) rest
          | i + 1 < bufsize && ordf >  0xffff = do pokeElemOff buf i word1
                                                   pokeElemOff buf (i+1) word2
                                                   fillbuf (i+2) rest
          | otherwise                         = emptybuf buf i remain
          where ordf   = ord first
                asWord = fromInteger (toInteger ordf) :: CWchar
                sub    = ordf - 0x10000
                word1' = ((shiftR sub 10) .&. 0x3ff) + 0xD800
                word2' = (sub .&. 0x3FF)             + 0xDC00
                word1  = fromInteger . toInteger $ word1'
                word2  = fromInteger . toInteger $ word2'


        emptybuf :: (Ptr CWchar) -> Int -> [Char] -> IO ()
        emptybuf _ 0 []     = return ()
        emptybuf _ 0 remain = fillbuf 0 remain
        emptybuf ptr nLeft remain
          = do let nLeft'    = fromInteger . toInteger $ nLeft
               ret          <- win32WriteConsoleW handle ptr nLeft' written nullPtr
               nWritten     <- peek written
               let nWritten' = fromInteger . toInteger $ nWritten
               if ret && (nWritten > 0)
                  then emptybuf (ptr `plusPtr` (nWritten' * szWChar)) (nLeft - nWritten') remain
                  else fail "WriteConsoleW failed.\n"

    in  fillbuf 0 string

szWChar = sizeOf (0 :: CWchar)

makeConsoleInfo :: DWORD -> Handle -> IO (Either ConsoleInfo Handle)
makeConsoleInfo nStdHandle fallback
  = do handle     <- win32GetStdHandle nStdHandle
       is_console <- is_a_console handle
       let bufsize = 10000
       if not is_console then return $ Right fallback
         else do buf     <- mallocBytes (szWChar * bufsize)
                 written <- malloc
                 return . Left $ ConsoleInfo bufsize buf written handle

{-# NOINLINE stdoutConsoleInfo #-}
stdoutConsoleInfo :: Either ConsoleInfo Handle
stdoutConsoleInfo = unsafePerformIO $ makeConsoleInfo std_OUTPUT_HANDLE stdout

{-# NOINLINE stderrConsoleInfo #-}
stderrConsoleInfo :: Either ConsoleInfo Handle
stderrConsoleInfo = unsafePerformIO $ makeConsoleInfo std_ERROR_HANDLE stderr

interact     :: (String -> String) -> IO ()
interact f   = do s <- getContents
                  putStr (f s)

conPutChar ci  = writeConsole ci . replicate 1
conPutStr      = writeConsole
conPutStrLn ci = writeConsole ci . ( ++ "\n")

putChar      :: Char -> IO ()
putChar      = (either conPutChar  hPutChar ) stdoutConsoleInfo

putStr       :: String -> IO ()
putStr       = (either conPutStr   hPutStr  ) stdoutConsoleInfo

putStrLn     :: String -> IO ()
putStrLn     = (either conPutStrLn hPutStrLn) stdoutConsoleInfo

print        :: Show a => a -> IO ()
print        = putStrLn . show

getChar      = System.IO.getChar
getLine      = System.IO.getLine
getContents  = System.IO.getContents

readIO       :: Read a => String -> IO a
readIO       = System.IO.readIO

readLn       :: Read a => IO a
readLn       = System.IO.readLn

ePutChar     :: Char -> IO ()
ePutChar     = (either conPutChar  hPutChar ) stderrConsoleInfo

ePutStr     :: String -> IO ()
ePutStr      = (either conPutStr   hPutStr  ) stderrConsoleInfo

ePutStrLn   :: String -> IO ()
ePutStrLn    = (either conPutStrLn hPutStrLn) stderrConsoleInfo

ePrint       :: Show a => a -> IO ()
ePrint       = ePutStrLn . show

#else

import qualified System.IO
import Prelude (IO, Read, Show, String)

interact     = System.IO.interact
putChar      = System.IO.putChar
putStr       = System.IO.putStr
putStrLn     = System.IO.putStrLn
getChar      = System.IO.getChar
getLine      = System.IO.getLine
getContents  = System.IO.getContents
ePutChar     = System.IO.hPutChar System.IO.stderr
ePutStr      = System.IO.hPutStr System.IO.stderr
ePutStrLn    = System.IO.hPutStrLn System.IO.stderr

print        :: Show a => a -> IO ()
print        = System.IO.print

readIO       :: Read a => String -> IO a
readIO       = System.IO.readIO

readLn       :: Read a => IO a
readLn       = System.IO.readLn

ePrint       :: Show a => a -> IO ()
ePrint       = System.IO.hPrint System.IO.stderr

#endif

trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    traceIO string
    return expr

traceIO :: String -> IO ()
traceIO = ePutStrLn
