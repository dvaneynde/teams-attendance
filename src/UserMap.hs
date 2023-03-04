module UserMap
    ( 
    ) where

import System.Environment
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Text.Read
import Data.Time.Calendar
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace

data UserMap = UserMap 
    { name :: String
    , company :: String
    , eMails :: [String]
    } deriving (Show)

readUserMapFile :: String -> [UserMap]

sortByCompanyThenName :: [UserMap] -> [UserMap]
-- test = UserMap "Dirk" "dlvm" ["dirk@vaneynde.eu", "dirk@dlvmechanografie.eu"]
-- UserMap {name = "Dirk", company = "dlvm", eMails = ["dirk@vaneynde.eu","dirk@dlvmechanografie.eu"]}
--putStrLn (show test)