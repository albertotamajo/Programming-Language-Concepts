module Main where
import System.Environment
import Control.Exception
import System.IO
import Tokens
import Lib
import Grammar


--------------------------------------------------------------------LINE COMMAND-----------------------------------------------------------------------
main :: IO ()
main = do
         (sourcePath:args) <- getArgs
         catch (readAndEvalSource sourcePath) printIOException

readAndEvalSource :: String -> IO()
readAndEvalSource sourcePath = do
                                 sourceCode <- readFile sourcePath
                                 eval sourceCode
                                
                                                  
eval :: String -> IO()
eval xs = do
            let tokens = alexScanTokens xs
            case last tokens of
              TokenError e -> printParseException e xs
              _ -> do
                    program <- return (parser tokens)
                    case program of
                     Failed e -> printParseException e xs
                     Ok p -> do
                               b <- catch (typeCheck p) (\e -> printTypeCheckerException e xs)
                               case b of
                                 True -> do
                                           tableLists <- catch (evalProgram p) (\e -> printRunTimeException e)
                                           printTables tableLists
                                 False -> return ()

printTables :: [TableList] -> IO()
printTables [] = return ()
printTables (([],[],[]) : xs) = printTables xs
printTables ((head,types,rows) : xs) = do
                                        printRows rows
                                        printTables xs
                                        
                                                                             
printRows :: [[String]] -> IO ()
printRows [] = putStr ""
printRows (x:xs) = printRows' (x:xs)
                 where
                   printRows' [] = return ()
                   printRows' (x:xs) = do
                                        printRow x
                                        putStr "\n"
                                        printRows' xs
                    
printRow :: [String] -> IO ()
printRow [] = return ()
printRow (x:xs) = do
                    putStr x
                    if null xs then printRow xs else do
                                                       putStr ","
                                                       printRow xs

takeNSentence :: Int -> String -> String
takeNSentence n xs = let splitted = splitOn ';' xs in splitted !! (n-1)

printIOException :: IOException -> IO()
printIOException e = do 
                       let err = show e
                       hPutStr stderr ("Couldn't open source file: " ++ err)
                       return ()

printRunTimeException :: RunTimeException -> IO[TableList]
printRunTimeException e = do
                            hPutStr stderr (show e)
                            return []

printTypeCheckerException :: TypeCheckerException -> String -> IO Bool
printTypeCheckerException (TypeCheckerException n e) xs = do
                                                            hPutStr stderr ("\nTYPE CHECKER EXCEPTION\n" ++ takeNSentence n xs ++ "\n" ++ (show e) ++ "\n")
                                                            return False
                                                         
                                                         
printParseException :: ParseException -> String -> IO ()
printParseException (ParseException (line,col)) xs = hPutStr stderr ("\n" ++ show (ParseException (line,col)) ++ " => " ++ takeStringPos (line,col) xs ++ "\n" )
                                                       
takeStringPos :: (Int,Int) -> String -> String
takeStringPos (lin,col) xs = takeWhile (\x -> x /= '\n' && x /= ' ')(drop (col-1) rightLine)
                        where
                          splitted = splitOn '\n' xs
                          rightLine = splitted !! (lin-1)

--------------------------------------------------------------------LINE COMMAND-----------------------------------------------------------------------


------------------------------------------------------------------INTERPRETER CODE---------------------------------------------------------------------
{-
module Main where
import System.Environment
import System.Console.ANSI
import Control.Exception
import System.IO
import Tokens
import Lib
import Grammar

main :: IO ()
main = do
         setTitle "SQL-LIKE"
         main'

main' = do
         sourceCode <- getLines
         eval sourceCode
         main'



getLines :: IO String
getLines =  do
             setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
             putStr "SQL-LIKE> "
             setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
             hFlush stdout
             x <- getLine
             if last x == '.' then return (init x) else do
                                                  xs <- getLines
                                                  return (x ++ "\n" ++ xs)
                                                  
eval :: String -> IO()
eval xs = do
            let tokens = alexScanTokens xs
            case last tokens of
              TokenError e -> do
                                 printParseException e xs
                                 main'
              _ -> do
                    program <- return (parser tokens)
                    case program of
                     Failed e -> do
                                   printParseException e xs
                                   main'
                            
                     Ok p -> do
                               b <- catch (typeCheck p) (\e -> printTypeCheckerException e xs)
                               case b of
                                 True -> do
                                           tableLists <- evalProgram p
                                           printTables tableLists
                                 False -> main'

printTables :: [TableList] -> IO()
printTables [] = return ()
printTables (([],[],[]) : xs) = printTables xs
printTables ((head,types,rows) : xs) = do
                                        if null (filter (\x -> x /= "") head) then printRows rows else do
                                                                                                         printRow head
                                                                                                         putStr "\n"
                                                                                                         printRows rows
                                        
                                        
                                        
printRows :: [[String]] -> IO ()
printRows [] = return ()
printRows (x:xs) = do
                     printRow x
                     putStr "\n"
                     printRows xs
                    
printRow :: [String] -> IO ()
printRow [] = return ()
printRow (x:xs) = do
                    putStr x
                    if null xs then printRow xs else do
                                                       putStr ","
                                                       printRow xs

takeNSentence :: Int -> String -> String
takeNSentence n xs = let splitted = splitOn ';' xs in splitted !! (n-1)

printTypeCheckerException :: TypeCheckerException -> String -> IO Bool
printTypeCheckerException (TypeCheckerException n e) xs= do
                                                         setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
                                                         putStrLn "\nTYPE CHECKER EXCEPTION\n"
                                                         setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
                                                         putStrLn $ takeNSentence n xs ++ "\n"
                                                         setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
                                                         putStrLn $ (show e) ++ "\n"
                                                         setCursorColumn 0
                                                         return False
                                                         
                                                         
printParseException :: ParseException -> String -> IO ()
printParseException (ParseException (line,col)) xs = do
                                                         setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
                                                         putStr "\n"
                                                         putStr $ show (ParseException (line,col))
                                                         setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
                                                         putStr $ " => " ++ takeStringPos (line,col) xs ++ "\n"
                                                         putStr "\n"
                                                         return ()
                                                       
takeStringPos :: (Int,Int) -> String -> String
takeStringPos (lin,col) xs = takeWhile (\x -> x /= '\n' && x /= ' ')(drop (col-1) rightLine)
                        where
                          splitted = splitOn '\n' xs
                          rightLine = splitted !! (lin-1)
-}                         

------------------------------------------------------------------INTERPRETER CODE---------------------------------------------------------------------

                         
                                                        
                   