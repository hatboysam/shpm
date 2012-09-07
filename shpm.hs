import Control.Monad
import qualified Control.OldException as E
import System.IO
import System.Environment
import System.Directory
import Data.List

--A Simple Haskell Project Manager
--By: Sam Stern (2012)

main :: IO ()
main = do
	args <- getArgs
	dispatch args
	

dispatch :: [String] -> IO ()
dispatch (command:args)
	| (command == "add") = add args
	| (command == "remove") = remove args
	| (command == "list") = list
	| otherwise = unknownCommand command

dispatch _ = do
	putStrLn "Please enter a valid command"

add :: [String] -> IO ()
add (task:[]) = do
	addToFile task
	putStrLn $ "Added: " ++ task

add _ = do
	putStrLn "Can't add a blank task"

addToFile :: String -> IO ()
addToFile task = do
	appendFile "shpm.txt" $ task ++ "\n"

remove :: [String] -> IO ()
remove [] = do
	list
	putStrLn "Which # do you want to remove?"
	toRemove <- getLine
	remove [toRemove]
	

remove (toRemove:_) = do
	let toRemoveNum = read toRemove
	tasks <- getTasks
	let maxNum = (length tasks)
	if (toRemoveNum >= maxNum || toRemoveNum < 0)
		then putStrLn $ "Out of bounds: " ++ toRemove
		else replaceFile $ delete (tasks !! toRemoveNum) tasks

list :: IO ()
list = do
	tasks <- getTasks
	let	numberedTasks = zipWith (\num line -> (show num) ++ " - " ++ line) [0..] tasks
	if (numberedTasks == [])
		then putStrLn "No Tasks"
		else putStr $ unlines numberedTasks

getTasks :: IO [String]
getTasks = do
	fileContents <- (readFile "shpm.txt") `E.catch` (\_ -> return "")
	return $ lines fileContents

replaceFile :: [String] -> IO ()
replaceFile newTasks = do
	removeFile "shpm.txt"
	mapM_ addToFile newTasks


unknownCommand :: String -> IO ()
unknownCommand n = do
	putStrLn $ "Unknown command: " ++ n