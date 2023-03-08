module Main where

import TeamsAttendance
import UserMap
import Utils

import Options.Applicative
import Data.Semigroup ((<>))

-- TODO tests

data CommandLine = CommandLine
  { usermap     :: Maybe String
  , utf16       :: Bool
  , quiet       :: Bool
  , csvfiles    :: [String] } deriving Show

commandLine :: Parser CommandLine
commandLine = CommandLine
    <$> (optional $ strOption
        ( long "usermap"
        <> short 'u'
        <> metavar "FILE"
        <> help "Mapping of Teams e-mails to a company and username for reports. See Github, data/UserMap.txt for an example." ))
    <*> switch
        ( long "utf16"
        <> help "CSV files are in UTF-16." )
    <*> switch
        ( long "quiet"
        <> short 'q'
        <> help "Whether to be quiet." )
    <*> some (argument str (metavar "file.csv..."))


main :: IO ()
main = generateReport =<< execParser opts
  where
    opts = info (commandLine <**> helper)
      ( fullDesc
     <> progDesc "Given multiple Microsoft Teams meeting reports in .csv format, generate an overview of attendance."
     <> header "Aggregate MS Teams Attendance Report" )


generateReport :: CommandLine -> IO ()

generateReport (CommandLine _ _ True files) = 
    errPutStrLn "Sorry, quiet mode not implemented yet."  

generateReport (CommandLine mUserMap utf16 _ files) =
        generateAttendanceReport mUserMap utf16 files  

