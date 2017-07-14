import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

boardfileName = "./boardfile"

dispatch :: [String] -> IO ()
dispatch command@(c:argList)
    |c == "-h" = displayHelp
    |c == "read" = readPost argList
    |c == "post" = writePost argList
dispatch _ = displayHelp

main = do
    command <- getArgs
    dispatch command

readPost :: [String] -> IO ()
readPost _ = do
    contents <- readFile boardfileName
    putStr contents

writePost :: [String] -> IO ()
writePost [postText] = appendFile boardfileName (postText ++ "\n")

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