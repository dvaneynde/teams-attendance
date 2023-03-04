module Main where

import TeamsAttendance
import Utils

import Options.Applicative
import Data.Semigroup ((<>))

-- TODO tests

data CommandLine = CommandLine
  { usermap     :: Maybe String
  , quiet       :: Bool
  , csvfiles    :: [String] } deriving Show

commandLine :: Parser CommandLine
commandLine = CommandLine
    <$> (optional $ strOption
        ( long "usermap"
        <> short 'u'
        <> metavar "FILE"
        <> help "Mapping of Teams e-mails to a company and username for reports." ))
    <*> switch
        ( long "quiet"
        <> short 'q'
        <> help "Whether to be quiet" )
    <*> some (argument str (metavar "file.csv..."))


main :: IO ()
main = generateReport =<< execParser opts
  where
    opts = info (commandLine <**> helper)
      ( fullDesc
     <> progDesc "Given multiple Microsoft Teams meeting reports in .csv format, generate an overview of attendance."
     <> header "Aggregate MS Teams Attendance Report" )


generateReport :: CommandLine -> IO ()

generateReport (CommandLine _ True files) = 
    errPutStrLn "Sorry, quiet mode not implemented yet."  

generateReport (CommandLine (Just _) _ files) = 
    errPutStrLn "Sorry, usermap not implemented yet."  

generateReport (CommandLine u q files) =
    do
        -- putStrLn ("UserMapping: " ++ (show u))
        -- putStrLn ("Files: " ++ (unlines files))
        generateAttendanceReport files  

