module Utils
    ( errPutStrLn, readWholeFile
    ) where

import GHC.IO.Handle
import GHC.IO.Handle.FD

import GHC.IO.IOMode
import GHC.IO.Encoding.Types
import GHC.IO.Encoding.UTF16
import GHC.IO.Encoding.UTF8

import Data.Maybe

errPutStrLn :: String -> IO ()
errPutStrLn s = hPutStr stderr (s ++ "\n")


readWholeFile :: Bool -> String -> IO String
readWholeFile setUtf16 filename = do
    h <- openFile filename ReadMode
    defaultEncoding <- hGetEncoding h
    let savedEncoding = fromMaybe utf8 defaultEncoding
    hSetEncoding h (if setUtf16 then utf16 else savedEncoding)
    result <- hGetContents h
    return result

