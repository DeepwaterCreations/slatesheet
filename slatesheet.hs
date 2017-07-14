import System.Environment
import System.Directory
import System.IO
import System.Posix.User
import Data.Time.Clock
import Data.Time.LocalTime
import Data.List
import Control.Exception

topicFileName = "./topiclist"
boardFileName = "./boardfile"

dispatch :: [String] -> IO ()
dispatch command@(c:argList)
    |c == "-h" = displayHelp
    |c == "ls" = listTopics
    |c == "add" = addTopic argList
    |c == "read" = readPost argList
    |c == "post" = writePost argList
dispatch _ = displayHelp

main = do
    command <- getArgs
    dispatch command

listTopics :: IO ()
listTopics = do
    topics <- readFile topicFileName
    let topicList = lines topics
        numberedTopicList = zipWith (\n line -> show n ++ 
                                    " - " ++
                                    line)
                                    [0..] topicList
    putStrLn "TOPICS OF CAPRICORN"
    putStr $ unlines numberedTopicList

addTopic :: [String] -> IO ()
addTopic [topicName] = do
    appendFile topicFileName (topicName ++ "\n")    

readPost :: [String] -> IO ()
readPost _ = do
    contents <- readFile boardFileName
    putStr contents

writePost :: [String] -> IO ()
writePost [postText] = do
    loginName <- getLoginName
    currentTime <- getCurrentTime
    currentTimeZone <- getCurrentTimeZone
    let localTime = utcToLocalTime currentTimeZone currentTime
        postString = (show localTime) ++ "\n" ++ 
                    loginName ++ " wrote:\n" ++ 
                    postText ++ "\n\n"
    appendFile boardFileName postString

displayHelp = do
    putStrLn ""
    putStrLn "*****"
    putStrLn "SlateSheet - a bulletin board by Bismuth"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "slatesheet read - display the current state of the board"
    putStrLn "slatesheet post MESSAGETEXT - post MESSAGETEXT to the board"
    putStrLn ""
    putStrLn "(That's it right now. Come back later.)"
    putStrLn "*****"
