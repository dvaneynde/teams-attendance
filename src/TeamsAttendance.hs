module TeamsAttendance
    ( generateAttendanceReport
    ) where

import Text.Regex.TDFA
import Data.Time.Calendar
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace
import Utils
import UserMap


data DayInfo = DayInfo 
    { day :: Day
    , elapsed :: Minutes
    , average :: Minutes
    , users :: [UserInfo]
    } deriving (Show)
    
type Minutes = Int
data UserInfo = UserInfo String Int deriving (Show)


{-
Reads one or more Teams Attendance reports and makes statistics per day and user in a comma separated CSV standard output.

Example with 2 days:
Who,14/11,15/11
Elapsed,10,20.36
Average,10,15.3
Dirk,10,20.33
Jan,5,20
-}
generateAttendanceReport :: Maybe String -> Bool -> [String] -> IO ()
generateAttendanceReport mUserMapFilename isUtf16 csvFileNames = do
    -- errPutStrLn "The dates are:" ++ (mapM errPutStrLn (map showGregorian $ getDays args))
    results <- mapM (processCsvFile isUtf16) csvFileNames    -- IO [Either String DayInfo]
    mUserMaps <- readUserMapFile mUserMapFilename
    putStr $ createOutputReport mUserMaps results


processCsvFile :: Bool -> String -> IO (Either String DayInfo)
processCsvFile isUtf16 filename = 
    {- trace ("processing file: " ++ filename) -}
    case parseFilenameForDate filename of
        Left error -> return $ Left error
        Right day ->
            let
                contents :: IO [String]
                contents = fmap lines $ (readWholeFile isUtf16 filename)
            in fmap (extractDayInfoFromReport day) contents


-- Extract date from Teams generated filenam, e.g. "Architecture Standup - Attendance report 05-12-22.csv" gives "05-12-22".
parseFilenameForDate :: String -> Either String Day
parseFilenameForDate s =
    let        
        m = s =~ "([0-9]+)-([0-9]+)-([0-9]+)" :: (String,String,String,[String])
    in case m of 
        (_,"",_,_) -> Left ("Cannot find date in filename: "++s)
        (_,matched,_,[dayS,monthS,yearS]) -> 
            let
                day = read dayS
                month = read monthS
                year = read yearS
            in Right $ fromGregorian (year+2000) month day


-- Extract all information from one Attendance Teams report
extractDayInfoFromReport :: Day -> [String] -> Either String DayInfo
extractDayInfoFromReport day rows =
    do
        elapsed <- extractDuration "Meeting duration" rows
        average <- extractDuration "Average attendance" rows
        users <- extractUsers rows
        return DayInfo {day=day, elapsed=elapsed, average=average, users=users}
    -- in Right $ trace ("Extracted report: " ++ (show result)) result


createOutputReport :: Maybe [UserMap] -> [Either String DayInfo] -> String
createOutputReport mUserMaps errorOrInfos =
    case sequence errorOrInfos of 
        Left error -> ("Cannot generate report because: " ++ error)
        Right infos -> 
            let
                output1 = foldl (\acc dayInfo -> acc ++ ", " ++ (show $ day dayInfo)) "Who" infos
                output2 = foldl (\acc dayInfo -> acc ++ ", " ++ (show $ elapsed dayInfo)) (output1 ++ "\nElapsed") infos
                output3 = foldl (\acc dayInfo -> acc ++ ", " ++ (show $ average dayInfo)) (output2 ++ "\nAverage") infos

                {- for each user, output a line with their time, if present; if not present 0 -}
                uniqueEmails = getUniqueUsers infos
                uniqueUsers = case mUserMaps of
                    Nothing -> userMapsFromEmailsOnly uniqueEmails
                    Just userMaps -> addUnknownEmailsToUserMaps uniqueEmails userMaps
                sortedUniqueUsers = sortByCompanyThenName uniqueUsers
                userLines = map (createUserOverview infos) sortedUniqueUsers

            in 
                output3 ++ "\n" ++ (unlines userLines)

            where
                getUniqueUsers :: [DayInfo] -> [String]
                getUniqueUsers dis = 
                    let
                        usersPerMeeting = do
                            di <- dis
                            return $ map (\(UserInfo name _) -> name) (users di)
                    in nub $ concat usersPerMeeting

                -- For a given user, return all its times per day
                createUserOverview :: [DayInfo] -> UserMap -> String
                createUserOverview dis um = let
                        prefix = (show $ company um) ++ ", " ++ (show $ name um)
                    in
                        foldl (\a di -> (a ++ (processDI di))) prefix dis
                    where
                        processDI :: DayInfo -> String
                        processDI di =
                            case find (\(UserInfo name _) -> name `elem` (eMails um)) (users di) of
                                Nothing -> ", 0"
                                Just (UserInfo _ time) -> ", " ++ (show time)


-- Parses strings of form "1h 3s", "25m 30s", "30s" etc. and gives time elapsed in seconds, or Nothing if not correct format
convertTeamsTimeToSecs :: String -> Either String Int
convertTeamsTimeToSecs s =
    let
        s' = " " ++ s ++ " "
        m = s' =~ "\\` *(([0-9]+)h )?(([0-9]+)m )?(([0-9]+)s)? *\\'" :: (String, String, String, [String])
        
        number :: String -> Int
        number s = case s of
            "" -> 0
            _ -> read s

    in case m of 
        (_,"",_,_) -> Left ("Failed to interpret Teams Time: "++ s)
        (_,_,_,[_,"",_,"",_,""]) -> Left ("Could not parse Teams Time: " ++ s)
        (_,_,_,[_,hourS,_,minS,_,secS]) -> Right $ ((number hourS) * 60 + (number minS)) * 60 + (number secS)


-- Extract Teams formatted duration from given rows that has given prefix
extractDuration :: String -> [String] -> Either String Int
extractDuration prefix rows =
    let
        line = head $ filter (isPrefixOf prefix) rows
        durationString = dropWhile (not . isDigit) line
    in  convertTeamsTimeToSecs durationString


-- Extract UserInfo from a Teams report line
extractUserInfo :: String -> Either String UserInfo
extractUserInfo line =
    let 
        parts = splitOn "\t" line
    in if ((length parts) < 5)
        then Left ("User info line has less than 5 tab-separated parts; line=" ++ line)
        else do
            duration <- convertTeamsTimeToSecs (parts !! 3) 
            let userEmail = takeWhile (not . isSpace) $ (dropWhile isSpace $ parts !! 4)
            return $ UserInfo userEmail duration


-- Extract all UserInfos from a Teams report in rows
extractUsers :: [String] -> Either String [UserInfo]
extractUsers rows =
    let
        rows1 = tail $ dropWhile (not . (isPrefixOf "Name")) rows
        rows2 = takeWhile (not . all isSpace) rows1
        --result = trace ("rows:\n" ++ (unlines rows2) ++ "end rows\n") []
        result = map extractUserInfo rows2
    in sequence result


getDays :: [String] -> [Day]
getDays ss =
    case mapM parseFilenameForDate ss of
        Left s -> []
        Right dd -> dd
