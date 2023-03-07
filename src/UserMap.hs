module UserMap where

import Data.List
import Data.Sort
import Data.Function


data UserMap = UserMap 
    { name :: String
    , company :: String
    , eMails :: [String]
    } deriving (Eq, Show, Read)


-- Reads a file containing multiple of the following lines:
--   UserMap {name = "Dirk", company = "dlvm", eMails = ["dirk@vaneynde.eu","dirk@dlvmechanografie.eu"]}
-- TODO check that email appears in only one UserMap
readUserMapFile :: Maybe String -> IO (Maybe [UserMap])
readUserMapFile mFilename = 
    case mFilename of 
        Nothing -> return Nothing
        Just fileName -> do
            contents <- (fmap lines . readFile) fileName
            return $ Just $ map read contents


sortByCompanyThenName :: [UserMap] -> [UserMap]
sortByCompanyThenName userMaps =
    sortBy ((compare `on` company) <> (compare `on` name)) userMaps


userMapsFromEmailsOnly :: [String] -> [UserMap]
userMapsFromEmailsOnly userNames =
    map createUserMap userNames
    where
        createUserMap name = UserMap name "_Unknown" [name]


-- assumed that eMails are unique
addUnknownEmailsToUserMaps :: [String] -> [UserMap] -> [UserMap]
addUnknownEmailsToUserMaps ems ums =
    foldl (\ums eMail -> addIfNotPresent ums eMail) ums ems -- 	(a -> b -> a) -> a -> [b] -> a
    where
        addIfNotPresent :: [UserMap] -> String -> [UserMap]
        addIfNotPresent ums em = 
            case find (\um -> em `elem` (eMails um)) ums of
                Just _ -> ums
                Nothing -> UserMap em "_Unknown" [em] : ums

