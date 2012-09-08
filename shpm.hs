import Control.Monad
import qualified Control.Exception as E
import System.IO
import System.Environment
import System.Directory
import System.Console.ANSI
import Data.List
import Data.Ord

--A Simple Haskell Project Manager
--By: Sam Stern (2012)

main :: IO ()
main = do
	args <- getArgs
	dispatch args
	
--For ease of reading
type Task = String
type ProjName = String
type Project = (ProjName, [Task])

dispatch :: [String] -> IO ()
dispatch (command:args)
	| (command == "add") = add args
	| (command == "remove") = remove args
	| (command == "list") = list args
	| otherwise = unknownCommand command

dispatch _ = do
	putStrLn "Please enter a valid command"

add :: [String] -> IO ()
add (task:[]) = do
	addToProject "*Other" task
	putStrLn $ "Added: " ++ task
add (pname:task:[]) = do
	addToProject ("*" ++ pname) task
	putStrLn $ "Added: " ++ task
add [] = do
	putStrLn "Can't add a blank task"
add _ = do
	putStrLn "Incorrect # of arguments"

addToProject :: ProjName -> Task -> IO ()
addToProject name task = do
	projects <- getProjects
	let newProjects = addToProject' projects name task
	replaceFile $ flattenProjects newProjects

addToProject' :: [Project] -> ProjName -> Task -> [Project]
addToProject' [] pname task = [(pname, [task])]
addToProject' ((name,tasks):xs) pname task
	| (name == pname) = (name, (tasks ++ [task])):xs
	| otherwise = (name,tasks):(addToProject' xs pname task)

remove :: [String] -> IO ()
remove [] = do
	list ["-nc"]
	putStrLn "Which # do you want to remove?"
	toRemove <- getLine
	remove [toRemove]
remove (toRemove:_) = do
	--TODO catch read error
	let toRemoveNum = read toRemove :: Int
	projects <- getProjects
	let maxNum = foldl (\a b -> a + (numTasks b)) 0 projects
	if (toRemoveNum >= maxNum || toRemoveNum < 0)
		then putStrLn $ "Out of bounds: " ++ toRemove
		else replaceFile (flattenProjects $ removeTask projects toRemoveNum)

removeTask :: [Project] -> Int -> [Project]
removeTask (x:xs) num
	| num < numTasks x = (deleteTask num x):xs
	| otherwise = x:(removeTask xs (num - numTasks x))
	where deleteTask num (name,tasks) = (name, (delete (tasks !! num) tasks)) 

list :: [String] -> IO ()
list (option:_) = do
	let useColor = (option /= "-nc")
	projects <- getProjects
	printProjects projects useColor
list [] = do
	projects <- getProjects
	printProjects projects True

printProjects :: [Project] -> Bool -> IO ()
printProjects projects color = printProjects' projects color 0

printProjects' :: [Project] -> Bool -> Int -> IO ()
printProjects' [] _ _ = do
	return ()
printProjects' (x:xs) color num = do
	printProject x num color
	printProjects' xs color (num + (numTasks x))

numTasks :: Project -> Int
numTasks (name, tasks) = length tasks

printProject :: Project -> Int -> Bool -> IO ()
printProject (name, tasks) num colors = do
	let prettyName = (tail name) ++ ":"
	let numberedTasks = zipWith (\num t -> (show num) ++ " - " ++ t) [num..] tasks
	let tabbedTasks = map (\t -> "  " ++ t) numberedTasks
	if (colors)
		then putStrLnColor Red prettyName
		else putStrLn prettyName
	if (colors)
		then mapM_ (putStrLnColor Yellow) tabbedTasks
		else mapM_ putStrLn tabbedTasks

putStrLnColor :: Color -> String -> IO ()
putStrLnColor c s = do
	setSGR [ SetColor Foreground Vivid c ]
	putStrLn s
	setSGR [ Reset ]

getAllLines :: IO [String]
getAllLines = do
	fileContents <- (readFile "shpm.txt") `E.catch` (\e -> const (return "") (e :: E.IOException))
	return $ lines fileContents

getProjects :: IO [Project]
getProjects = do
	fileContents <- (readFile "shpm.txt") `E.catch` (\e -> const (return "") (e :: E.IOException))
	let allLines = lines fileContents
	return $ sortProjects (parseFile allLines)

parseFile :: [String] -> [Project]
parseFile l = parseFile' l []

parseFile' :: [String] -> [Project] -> [Project]
parseFile' [] l = l
parseFile' (x:xs) l
	| (length x == 0) = parseFile' xs l
	| ((x !! 0) == '*') = parseFile' xs $ l ++ [(x, [])]
	| otherwise = parseFile' xs $ (init l) ++ [addTaskTo (last l) x]
	where addTaskTo (name, tasks) x = (name, tasks ++ [x])

flattenProjects :: [Project] -> [String]
flattenProjects projects = map (\(name, tasks) -> name ++ "\n" ++ (flattenTasks tasks)) $ sortProjects (clearBlanks projects)
	where flattenTasks tasks = foldl (\a b -> a ++ "\n" ++ b) "" tasks

sortProjects :: [Project] -> [Project]
sortProjects projects = sortBy compareProjects projects

compareProjects :: Project -> Project -> Ordering
compareProjects (name1,_) (name2,_)
	| name1 == "*Other" = GT
	| name2 == "*Other" = LT
	| name1 > name2 = GT
	| name1 < name2 = LT
	| otherwise = EQ

clearBlanks :: [Project] -> [Project]
clearBlanks [] = []
clearBlanks (x:xs)
	| numTasks x == 0 = clearBlanks xs
	| otherwise = x:(clearBlanks xs)

replaceFile :: [String] -> IO ()
replaceFile newTasks = do
	--TODO sort projects alphabetically
	removeFile "shpm.txt" `E.catch` (\e -> const (return ()) (e :: E.IOException))
	mapM_ addToFile newTasks

addToFile :: String -> IO ()
addToFile task = do
	appendFile "shpm.txt" $ task ++ "\n"

unknownCommand :: String -> IO ()
unknownCommand n = do
	putStrLn $ "Unknown command: " ++ n

