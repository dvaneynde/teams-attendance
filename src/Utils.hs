module Utils
    ( errPutStrLn
    ) where

import GHC.IO.Handle
import GHC.IO.Handle.FD

errPutStrLn :: String -> IO ()
errPutStrLn s = hPutStr stderr (s ++ "\n")


