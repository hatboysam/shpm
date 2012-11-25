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
	makeFileIfNecessary
	args <- getArgs
	dispatch args
	
--For ease of reading
type Task = (String, Bool)
type ProjName = String
type Project = (ProjName, [Task])

dispatch :: [String] -> IO ()
dispatch (command:args)
	| (command == "add") = add args
	| (command == "addp") = addp args
	| (command == "remove") = remove args
	| (command == "list") = list args
	| (command == "loc") = loc
	| (command == "help") = help
	| (command == "pri") = pri args
	| otherwise = unknownCommand command

dispatch _ = do
	putStrLn "Please enter a valid command"

add :: [String] -> IO ()
add (task:[]) = do
	addToProject "*Other" (task, False)
	putStrLn $ "Added: " ++ task
add (pname:task:[]) = do
	addToProject ("*" ++ pname) (task, False)
	putStrLn $ "Added: " ++ task
add [] = do
	putStrLn "Can't add a blank task"
add _ = do
	putStrLn "Incorrect # of arguments"

addp :: [String] -> IO ()
addp (task:[]) = do
	add ['^':task]
addp (pname:task:[]) = do
	add (pname:('^':task):[])
addp [] = do
	add []
addp _ = do
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
	let maxNum = maxTaskNum projects
	if (toRemoveNum >= maxNum || toRemoveNum < 0)
		then putStrLn $ "Out of bounds: " ++ toRemove
		else replaceFile (flattenProjects $ removeTask projects toRemoveNum)

maxTaskNum :: [Project] -> Int
maxTaskNum projects = foldl (\a b -> a + (numTasks b)) 0 projects

removeTask :: [Project] -> Int -> [Project]
removeTask (x:xs) num
	| num < numTasks x = (deleteTask num x):xs
	| otherwise = x:(removeTask xs (num - numTasks x))
	where deleteTask num (name,tasks) = (name, (delete (tasks !! num) tasks)) 

priTask :: [Project] -> Int -> [Project]
priTask (x:xs) num
	| num < numTasks x = (fst x, (priTask' num (snd x))):xs
	| otherwise = x:(priTask xs (num - numTasks x))

priTask' :: Int -> [Task] -> [Task]
priTask' num (task:xs)
	| (num /= 0) = task:(priTask' (num - 1) xs)
	| otherwise = (fst task, True):xs 

list :: [String] -> IO ()
list (option:_) = do
	let useColor = (option /= "-nc")
	projects <- getProjects
	printProjects projects useColor
list [] = do
	projects <- getProjects
	printProjects projects True

loc :: IO ()
loc = do
	path <- textPath
	putStrLn path

pri :: [String] -> IO ()
pri (number:_) = do
	let toPriNum = read number :: Int
	projects <- getProjects
	let maxNum = maxTaskNum projects
	if (toPriNum >= maxNum || toPriNum < 0)
		then putStrLn $ "Out of bounds: " ++ number
		else do
			replaceFile (flattenProjects $ priTask projects toPriNum)
			putStrLn $ "Prioritized task #" ++ number
pri _ = do
	putStrLn "Error, must speciffy a number" 

help :: IO ()
help = do
	putStrLn "-----------------------------"
	putStrLn " *****  *    *  ****   *   * "
	putStrLn " *      *    *  *   *  ** ** "
	putStrLn " *****  ******  ****   * * * "
	putStrLn "     *  *    *  *      *   * "
	putStrLn " *****  *    *  *      *   * "
	putStrLn "-----------------------------"
	putStrLn "add {task} -> add task to project Other"
	putStrLn "add {proj} {task} -> add task to project proj"
	putStrLn "pri {n} -> prioritize task #n (prints in different color)"
	putStrLn "addp -> same arguments/options as add, prioritizes new task"
	putStrLn "list -> list all tasks (in color)"
	putStrLn "list -nc -> list all tasks (no color)"
	putStrLn "remove -> list all tasks and choose one to remove"
	putStrLn "remove {n} -> remove task #n from the list"
	putStrLn "loc -> show location of tasks file"
	putStrLn "help -> show this menu"
	putStrLn ""

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
	let numberedTasks = zipWith (\num t -> ((show num) ++ " - " ++ (cutCarat t), snd t)) [num..] $ tasks
	let tabbedTasks = map (\t -> (("  " ++ fst t), snd t)) numberedTasks
	if (colors)
		then putStrLnColor Red prettyName
		else putStrLn prettyName
	if (colors)
		then mapM_ (printTaskColor) tabbedTasks
		else mapM_ putStrLn $ firsts tabbedTasks
	where 
		cutCarat (string, False) = string -- Removes the "^" from pri tasks
		cutCarat (string, True) = (tail string)

printTaskColor :: Task -> IO ()
printTaskColor task = do
	if (snd task)
		then putStrLnColor Green $ fst task 
		else putStrLnColor Yellow $ fst task

firsts :: [(a,b)] -> [a]
firsts list = map (\(a,b) -> a) list

putStrLnColor :: Color -> String -> IO ()
putStrLnColor c s = do
	setSGR [ SetColor Foreground Dull c ]
	putStrLn s
	setSGR [ Reset ]

getAllLines :: IO [String]
getAllLines = do
	filePath <- textPath
	fileContents <- (readFile filePath) `E.catch` (\e -> const (return "") (e :: E.IOException))
	return $ lines fileContents

getProjects :: IO [Project]
getProjects = do
	filePath <- textPath	
	fileContents <- (readFile filePath) `E.catch` (\e -> const (return "") (e :: E.IOException))
	let allLines = lines fileContents
	return $ sortProjects (parseFile allLines)

parseFile :: [String] -> [Project]
parseFile l = parseFile' l []

parseFile' :: [String] -> [Project] -> [Project]
parseFile' [] l = l
parseFile' (x:xs) l
	| (length x == 0) = parseFile' xs l --Blank
	| ((x !! 0) == '*') = parseFile' xs $ l ++ [(x, [])] --Project
	| ((x !! 0) == '^') = parseFile' xs $ (init l) ++ [addTaskTo (last l) x True] --Priorty
	| otherwise = parseFile' xs $ (init l) ++ [addTaskTo (last l) x False] -- Regular Task
	where 
		addTaskTo (name, tasks) x pri = (name, tasks ++ [(x, pri)])

flattenProjects :: [Project] -> [String]
--TODO For some reason, I didn't have to add a newline after name, not sure why...
flattenProjects projects = map (\(name, tasks) -> name ++ (flattenTasks tasks)) $ sortProjects (clearBlanks projects)
	where 
		flattenTasks tasks = foldl (\a b -> a ++ "\n" ++ (flattenSingle b)) "" $ tasks
		flattenSingle (task, True) = 
			if((task !! 0) == '^')
				then task
				else '^':task
		flattenSingle (task, False) = task

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
	filePath <- textPath
	removeFile filePath `E.catch` (\e -> const (return ()) (e :: E.IOException))
	mapM_ addToFile newTasks

addToFile :: String -> IO ()
addToFile task = do
	filePath <- textPath
	appendFile filePath $ task ++ "\n"

textPath :: IO FilePath
textPath = do
	appDir <- getAppUserDataDirectory "shpm"
	return $ appDir ++ "/shpm.txt"

makeFileIfNecessary :: IO ()
makeFileIfNecessary = do
	appDir <- getAppUserDataDirectory "shpm"
	createDirectoryIfMissing True appDir

unknownCommand :: String -> IO ()
unknownCommand n = do
	putStrLn $ "Unknown command: " ++ n

