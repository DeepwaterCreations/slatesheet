import System.Environment
import System.Directory
import System.IO
import System.Posix.User
import Data.Time.Clock
import Data.Time.LocalTime
import Data.List
import Control.Exception

topicFileName = "/home/Quarry/code/slatesheet/topiclist"
topicFolderName = "/home/Quarry/code/slatesheet/topics/"

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
    numberedTopicList <- getNumberedTopics
    putStrLn "TOPICS OF CAPRICORN"
    putStr $ unlines numberedTopicList

addTopic :: [String] -> IO ()
addTopic [topicName] = do
    appendFile topicFileName (topicName ++ "\n")    

readPost :: [String] -> IO ()
readPost [topicIdxStr] = do
    let topicIdx = read topicIdxStr
    topicFileName <- getTopicFile topicIdx
    contents <- readFile topicFileName
    putStr contents

writePost :: [String] -> IO ()
writePost [topicIdxStr, postText] = do
    let topicIdx = read topicIdxStr
    topicFileName <- getTopicFile topicIdx
    loginName <- getLoginName
    currentTime <- getCurrentTime
    currentTimeZone <- getCurrentTimeZone
    let localTime = utcToLocalTime currentTimeZone currentTime
        postString = (show localTime) ++ "\n" ++ 
                    loginName ++ " wrote:\n" ++ 
                    postText ++ "\n\n"
    appendFile topicFileName postString

displayHelp = do
    putStrLn ""
    putStrLn "*****"
    putStrLn "SlateSheet - a bulletin board by Bismuth"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "slatesheet ls - list the topics"
    putStrLn "slatesheet add TOPICNAME - add a topic"
    putStrLn "slatesheet read TOPICNUMBER - display the contents of the indicated topic"
    putStrLn "slatesheet post TOPICNUMBER MESSAGETEXT - post MESSAGETEXT to the indicated topic"
    putStrLn ""
    putStrLn "(That's it right now. Come back later.)"
    putStrLn "*****"

getNumberedTopics :: IO [String]
getNumberedTopics = do
    topics <- readFile topicFileName
    let topicList = lines topics
        numberedTopicList = zipWith (\n line -> show n ++ 
                                    " - " ++
                                    line)
                                    [0..] topicList
    return numberedTopicList

getTopicFile :: Int -> IO String
getTopicFile topicIdx = do
    topics <- readFile topicFileName
    let topicsList = lines topics
        topicName = (topicsList !! topicIdx)
        topicFileName = topicFolderName ++ topicName
    return topicFileName
