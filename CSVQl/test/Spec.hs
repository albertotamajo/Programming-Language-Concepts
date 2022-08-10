import Test.QuickCheck
import Tokens
import Lib
import Grammar

tokeniseAndParse :: String -> Program
tokeniseAndParse xs = case let tokens = alexScanTokens xs in parser tokens of
                        Ok p -> p
                        Failed _ -> error "Error"

--Function that assers the type
--   The first argument is the name of the table in the import statement
--   The second argument is the list of types of the table
--   The third argument is the the header of the list
--   The fourth argument is a sentence or multiple sentences to check against
--   The fifth argument is the expected list type
assertType :: String -> [Type] -> [String] -> String -> [Type] -> Bool 
assertType tableName types header sentence expectedTypes | null header = typeOfProgram parsedWithoutHeader == ([Void] ++ expectedTypes)
                                                         | otherwise = typeOfProgram parsedWithHeader == ([Void] ++ expectedTypes)
                                                         where
                                                           typesImport = "("++ (tail $ init $ show types) ++ ")"
                                                           headerImport = "("++ (headerConstruct header) ++ ")"
                                                           inputWithHeader = "IMPORT 'hello.csv' :: " ++ typesImport ++ " " ++ headerImport ++ " AS " ++ tableName ++ ";\n" ++ sentence
                                                           inputWithoutHeader = "IMPORT 'hello.csv' :: " ++ typesImport ++ " AS " ++ tableName ++ ";\n" ++ sentence
                                                           parsedWithHeader = tokeniseAndParse inputWithHeader
                                                           parsedWithoutHeader = tokeniseAndParse inputWithoutHeader  
                                                           
                                                           headerConstruct :: [String] -> String
                                                           headerConstruct [] = ""
                                                           headerConstruct (x:[]) = x ++ (headerConstruct [])
                                                           headerConstruct (x:xs) = x ++ "," ++ headerConstruct (xs) 

expectTypeFailure ::  String -> [Type] -> [String] -> String -> [Type] -> Property
expectTypeFailure tableName types header sentence expectedTypes | null header = expectFailure (typeOfProgram parsedWithoutHeader == ([Void] ++ expectedTypes))
                                                                | otherwise = expectFailure (typeOfProgram parsedWithHeader == ([Void] ++ expectedTypes))
                                                         where
                                                           typesImport = "("++ (tail $ init $ show types) ++ ")"
                                                           headerImport = "("++ (headerConstruct header) ++ ")"
                                                           inputWithHeader = "IMPORT 'hello.csv' :: " ++ typesImport ++ " " ++ headerImport ++ " AS " ++ tableName ++ ";\n" ++ sentence
                                                           inputWithoutHeader = "IMPORT 'hello.csv' :: " ++ typesImport ++ " AS " ++ tableName ++ ";\n" ++ sentence
                                                           parsedWithHeader = tokeniseAndParse inputWithHeader
                                                           parsedWithoutHeader = tokeniseAndParse inputWithoutHeader  
                                                           
                                                           headerConstruct :: [String] -> String
                                                           headerConstruct [] = ""
                                                           headerConstruct (x:[]) = x ++ (headerConstruct [])
                                                           headerConstruct (x:xs) = x ++ "," ++ headerConstruct (xs) 
                                                           
--Function that assers the type
--   The first argument is the name of the table in the import statement
--   The second argument is the list of types of the table
--   The third argument is the the header of the list
--   The fourth argument is a sentence or multiple sentences to check against
--   The fifth argument is the expected list type
showType ::  String -> [Type] -> [String] -> String -> [Type]
showType tableName types header sentence | null header = typeOfProgram parsedWithoutHeader
                                         | otherwise = typeOfProgram parsedWithHeader 
                                         where
                                           typesImport = "("++ (tail $ init $ show types) ++ ")"
                                           headerImport = "("++ (headerConstruct header) ++ ")"
                                           inputWithHeader = "IMPORT 'hello.csv' :: " ++ typesImport ++ " " ++ headerImport ++ " AS " ++ tableName ++ ";\n" ++ sentence
                                           inputWithoutHeader = "IMPORT 'hello.csv' :: " ++ typesImport ++ " AS " ++ tableName ++ ";\n" ++ sentence
                                           parsedWithHeader = tokeniseAndParse inputWithHeader
                                           parsedWithoutHeader = tokeniseAndParse inputWithoutHeader   

                                           headerConstruct :: [String] -> String
                                           headerConstruct [] = ""
                                           headerConstruct (x:[]) = x ++ (headerConstruct [])
                                           headerConstruct (x:xs) = x ++ "," ++ headerConstruct (xs)  

--Function that asserts the query result against the result of sqlite
--   The first argument is the source path of the expected result
--   The second argument is the source path of the csv file to open
--   The second argument is the name of the table in the import statement
--   The third argument is the list of types of the table
--   The fourth argument is the the header of the list
--   The fifth argument is a sentence or multiple sentences to check against
assertQueryResult :: String -> String -> String -> [Type] -> [String] -> String -> Property  
assertQueryResult source csvFile tableName types header sentence  | null header = ioProperty propWithoutHeader 
                                                                  | otherwise = ioProperty propWithHeader
 
                                                                where
                                                                  typesImport = "("++ (tail $ init $ show types) ++ ")"
                                                                  headerImport = "("++ (headerConstruct header) ++ ")"
                                                                  inputWithHeader = "IMPORT '" ++ csvFile ++ "' :: " ++ typesImport ++ " " ++ headerImport ++ " WITH HEADER AS " ++ tableName ++ ";\n" ++ sentence
                                                                  inputWithoutHeader = "IMPORT '" ++ csvFile ++ "' :: " ++ typesImport ++ " WITH HEADER AS " ++ tableName ++ ";\n" ++ sentence
                                                                  parsedWithHeader = tokeniseAndParse inputWithHeader
                                                                  parsedWithoutHeader = tokeniseAndParse inputWithoutHeader   

                                                                  headerConstruct :: [String] -> String
                                                                  headerConstruct [] = ""
                                                                  headerConstruct (x:[]) = x ++ (headerConstruct [])
                                                                  headerConstruct (x:xs) = x ++ "," ++ headerConstruct (xs)   
                                                                  propWithHeader = do
                                                                                    eval <- evalProgram parsedWithHeader
                                                                                    let (_,_,rows) = last eval
                                                                                    --putStrLn "Result"
                                                                                    --putStrLn $ show $ rows
                                                                                    expectedRows <- readCsvFile source
                                                                                    --putStrLn "Expected Rows"
                                                                                    --putStrLn $ show $expectedRows
                                                                                    return $ rows == expectedRows
                                                                                    
                                                                  propWithoutHeader = do
                                                                                    eval <- evalProgram parsedWithoutHeader
                                                                                    let (_,_,rows) = last eval
                                                                                    --putStrLn "Result"
                                                                                    --putStrLn $ show $ rows
                                                                                    expectedRows <- readCsvFile source
                                                                                    --putStrLn "Expected Rows"
                                                                                    --putStrLn $ show $ expectedRows
                                                                                    return $ rows == expectedRows   

assertQueryResultWithLength :: Int -> String -> String -> [Type] -> [String] -> String -> Property  
assertQueryResultWithLength n csvFile tableName types header sentence  | null header = ioProperty propWithoutHeader 
                                                                       | otherwise = ioProperty propWithHeader
 
                                                                where
                                                                  typesImport = "("++ (tail $ init $ show types) ++ ")"
                                                                  headerImport = "("++ (headerConstruct header) ++ ")"
                                                                  inputWithHeader = "IMPORT '" ++ csvFile ++ "' :: " ++ typesImport ++ " " ++ headerImport ++ " WITH HEADER AS " ++ tableName ++ ";\n" ++ sentence
                                                                  inputWithoutHeader = "IMPORT '" ++ csvFile ++ "' :: " ++ typesImport ++ " WITH HEADER AS " ++ tableName ++ ";\n" ++ sentence
                                                                  parsedWithHeader = tokeniseAndParse inputWithHeader
                                                                  parsedWithoutHeader = tokeniseAndParse inputWithoutHeader   

                                                                  headerConstruct :: [String] -> String
                                                                  headerConstruct [] = ""
                                                                  headerConstruct (x:[]) = x ++ (headerConstruct [])
                                                                  headerConstruct (x:xs) = x ++ "," ++ headerConstruct (xs)   
                                                                  propWithHeader = do
                                                                                    (x:(_,_,rows):xs) <- evalProgram parsedWithHeader
                                                                                    return $ length rows == n
                                                                                    
                                                                  propWithoutHeader = do
                                                                                    (x:(_,_,rows):xs) <- evalProgram parsedWithoutHeader
                                                                                    return $ length rows == n
                                                                                    
                                                           

-- Function that given a list outputs it between 

main :: IO ()
main = do
        ----------------------------------------------------------------Tests for the tokenisation---------------------------------------------------------------------
        putStrLn "Tests for the tokenisation"
        putStrLn "Test #1"
        quickCheck (alexScanTokens "IMPORT" == [TokenImport (AlexPn 0 1 1)])
        putStrLn "Test #2"
        quickCheck (alexScanTokens "AS" == [TokenAs (AlexPn 0 1 1)])
        putStrLn "Test #3"
        quickCheck (alexScanTokens ";" == [TokenEndSentence (AlexPn 0 1 1)])
        putStrLn "Test #4"
        quickCheck (alexScanTokens "::" == [TokenHasType (AlexPn 0 1 1)])
        putStrLn "Test #5"
        quickCheck (alexScanTokens "(" == [TokenLParen (AlexPn 0 1 1)])
        putStrLn "Test #6"
        quickCheck (alexScanTokens ")" == [TokenRParen (AlexPn 0 1 1)])
        putStrLn "Test #7"
        quickCheck (alexScanTokens ":=" == [TokenVarAssignment (AlexPn 0 1 1)])
        putStrLn "Test #8"
        quickCheck (alexScanTokens "Bool" == [TokenBool (AlexPn 0 1 1)])
        putStrLn "Test #9"
        quickCheck (alexScanTokens "Char" == [TokenChar (AlexPn 0 1 1)])
        putStrLn "Test #10"
        quickCheck (alexScanTokens "String" == [TokenString (AlexPn 0 1 1)])
        putStrLn "Test #11"
        quickCheck (alexScanTokens "Int" == [TokenInt (AlexPn 0 1 1)])
        putStrLn "Test #12"
        quickCheck (alexScanTokens "Float" == [TokenFloat (AlexPn 0 1 1)])
        putStrLn "Test #13"
        quickCheck (alexScanTokens "," == [TokenComa (AlexPn 0 1 1)])
        putStrLn "Test #14"
        quickCheck (alexScanTokens "CONCAT" == [TokenConcat (AlexPn 0 1 1)])
        putStrLn "Test #15"
        quickCheck (alexScanTokens "AND" == [TokenAnd (AlexPn 0 1 1)])
        putStrLn "Test #16"
        quickCheck (alexScanTokens "OR" == [TokenOr (AlexPn 0 1 1)])
        putStrLn "Test #17"
        quickCheck (alexScanTokens "NOT" == [TokenNot (AlexPn 0 1 1)])
        putStrLn "Test #18"
        quickCheck (alexScanTokens "=" == [TokenEqual (AlexPn 0 1 1)])
        putStrLn "Test #19"
        quickCheck (alexScanTokens "!=" == [TokenDifferent (AlexPn 0 1 1)])
        putStrLn "Test #20"
        quickCheck (alexScanTokens ">" == [TokenGreaterThan (AlexPn 0 1 1)])
        putStrLn "Test #21"
        quickCheck (alexScanTokens "<" == [TokenLessThan (AlexPn 0 1 1)])
        putStrLn "Test #22"
        quickCheck (alexScanTokens "<=" == [TokenLessThanEqual (AlexPn 0 1 1)])
        putStrLn "Test #23"
        quickCheck (alexScanTokens ">=" == [TokenGreaterThanEqual (AlexPn 0 1 1)])
        putStrLn "Test #24"
        quickCheck (alexScanTokens "+" == [TokenAddition (AlexPn 0 1 1)])
        putStrLn "Test #25"
        quickCheck (alexScanTokens "-" == [TokenSubtraction (AlexPn 0 1 1)])
        putStrLn "Test #26"
        quickCheck (alexScanTokens "/" == [TokenDivision (AlexPn 0 1 1)])
        putStrLn "Test #27"
        quickCheck (alexScanTokens "%" == [TokenModulo (AlexPn 0 1 1)])
        putStrLn "Test #28"
        quickCheck (alexScanTokens "^" == [TokenExponent (AlexPn 0 1 1)])
        putStrLn "Test #29"
        quickCheck (alexScanTokens "UNION" == [TokenUnion (AlexPn 0 1 1)])
        putStrLn "Test #30"
        quickCheck (alexScanTokens "MERGE" == [TokenMerge (AlexPn 0 1 1)])
        putStrLn "Test #31"
        quickCheck (alexScanTokens "INNER PRODUCT" == [TokenInnerProduct (AlexPn 0 1 1)])
        putStrLn "Test #32"
        quickCheck (alexScanTokens "LEFT PRODUCT" == [TokenLeftProduct (AlexPn 0 1 1)])
        putStrLn "Test #33"
        quickCheck (alexScanTokens "RIGHT PRODUCT" == [TokenRightProduct (AlexPn 0 1 1)])
        putStrLn "Test #34"
        quickCheck (alexScanTokens "FULL PRODUCT" == [TokenFullProduct (AlexPn 0 1 1)])
        putStrLn "Test #35"
        quickCheck (alexScanTokens "GET" == [TokenGet (AlexPn 0 1 1)])
        putStrLn "Test #36"
        quickCheck (alexScanTokens "GIVEN" == [TokenGiven (AlexPn 0 1 1)])
        putStrLn "Test #37"
        quickCheck (alexScanTokens "WHERE" == [TokenWhere (AlexPn 0 1 1)])
        putStrLn "Test #38"
        quickCheck (alexScanTokens "*" == [TokenStar (AlexPn 0 1 1)])
        putStrLn "Test #39"
        quickCheck (alexScanTokens "EMPTY" == [TokenEmpty (AlexPn 0 1 1)])
        putStrLn "Test #40"
        quickCheck (alexScanTokens "NOTEMPTY" == [TokenNotEmpty (AlexPn 0 1 1)])
        putStrLn "Test #41"
        quickCheck (alexScanTokens "SATISFIES" == [TokenSatisfies (AlexPn 0 1 1)])
        putStrLn "Test #42"
        quickCheck (alexScanTokens "BETWEEN" == [TokenBetween (AlexPn 0 1 1)])
        putStrLn "Test #43"
        quickCheck (alexScanTokens "NOTBETWEEN" == [TokenNotBetween (AlexPn 0 1 1)])
        putStrLn "Test #44"
        quickCheck (alexScanTokens "FIRST" == [TokenFirst (AlexPn 0 1 1)])
        putStrLn "Test #45"
        quickCheck (alexScanTokens "LAST" == [TokenLast (AlexPn 0 1 1)])
        putStrLn "Test #46"
        quickCheck (alexScanTokens "ORDERBY" == [TokenOrderBy (AlexPn 0 1 1)])
        putStrLn "Test #47"
        quickCheck (alexScanTokens "GROUPBY" == [TokenGroupBy (AlexPn 0 1 1)])
        putStrLn "Test #48"
        quickCheck (alexScanTokens "ASC" == [TokenAsc (AlexPn 0 1 1)])
        putStrLn "Test #49"
        quickCheck (alexScanTokens "DESC" == [TokenDesc (AlexPn 0 1 1)])
        putStrLn "Test #50"
        quickCheck (alexScanTokens "COUNT" == [TokenCount (AlexPn 0 1 1)])
        putStrLn "Test #51"
        quickCheck (alexScanTokens "SUM" == [TokenSum (AlexPn 0 1 1)])
        putStrLn "Test #52"
        quickCheck (alexScanTokens "AVG" == [TokenAvg (AlexPn 0 1 1)])
        putStrLn "Test #53"
        quickCheck (alexScanTokens "MIN" == [TokenMin (AlexPn 0 1 1)])
        putStrLn "Test #54"
        quickCheck (alexScanTokens "MAX" == [TokenMax (AlexPn 0 1 1)])
        putStrLn "Test #55"
        quickCheck (alexScanTokens "True" == [TokenTrue (AlexPn 0 1 1)])
        putStrLn "Test #56"
        quickCheck (alexScanTokens "False" == [TokenFalse (AlexPn 0 1 1)])
        putStrLn "Test #57"
        quickCheck (alexScanTokens "WITH HEADER" == [TokenHeader (AlexPn 0 1 1)])
        putStrLn "Test #58"
        quickCheck (alexScanTokens "'Trial.csv'" == [TokenTableSource (AlexPn 0 1 1) "Trial.csv"])
        putStrLn "Test #59"
        quickCheck (alexScanTokens "\"hello I am myself\"" == [TokenStringLit (AlexPn 0 1 1) "hello I am myself"])
        putStrLn "Test #60"
        quickCheck (alexScanTokens "'C2'" == [TokenColumnInt (AlexPn 0 1 1) 2 ])
        putStrLn "Test #61"
        quickCheck (alexScanTokens "'Users'" == [TokenColumnName (AlexPn 0 1 1) "Users"])
        putStrLn "Test #62"
        quickCheck (alexScanTokens "TablePen" == [TokenTable (AlexPn 0 1 1) "TablePen"])
        putStrLn "Test #63"
        quickCheck (alexScanTokens "x4" == [TokenVar (AlexPn 0 1 1) "x4"])
        putStrLn "Test #64"
        quickCheck (alexScanTokens "12.34" == [TokenFloatLit (AlexPn 0 1 1) 12.34])
        putStrLn "Test #65"
        quickCheck (alexScanTokens "12" == [TokenIntLit (AlexPn 0 1 1) 12])
        putStrLn "Test #66"
        quickCheck (alexScanTokens "Users " == [TokenTable (AlexPn 0 1 1) "Users"])
        putStrLn "Test #67"
        quickCheck (alexScanTokens "x4 " == [TokenVar (AlexPn 0 1 1)  "x4"])
        
        ----------------------------------------------------------------Tests for the Parser-----------------------------------------------------------
        putStrLn "Tests for the parser"
        putStrLn "Tests for the Import"
        putStrLn "Test #1"
        quickCheck (tokeniseAndParse "IMPORT 'hello.csv' :: (String,Int) AS Table" == [Import "hello.csv" [String,Int] ["",""] WithoutHeader "Table"])
        putStrLn "Test #2"
        quickCheck (tokeniseAndParse "IMPORT 'hello.csv' :: (String,Int) ('Id','Price') AS Table" == [Import "hello.csv" [String,Int] ["Id","Price"] WithoutHeader "Table"])
        putStrLn "Test #3"
        quickCheck (tokeniseAndParse "IMPORT 'hello.csv' :: (String,Int) WITH HEADER AS Table" == [Import "hello.csv" [String,Int] ["",""] WithHeader "Table"])
        putStrLn "Test #4"
        quickCheck (tokeniseAndParse "IMPORT 'hello.csv' :: (String,Int) ('Id','Price') WITH HEADER AS Table" == [Import "hello.csv" [String,Int] ["Id","Price"] WithHeader "Table"])
        
        putStrLn "Tests for the Queries"
        putStrLn "Test #1"
        quickCheck (tokeniseAndParse "GET Table(*)" == [S (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))])
        putStrLn "Test #2"
        quickCheck (tokeniseAndParse "(GET Table(*))" == [S (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))])
        putStrLn "Test #3"
        quickCheck (tokeniseAndParse "x" == [S (VarQuery "x")])
        putStrLn "Test #4"
        quickCheck (tokeniseAndParse "GET Table('C1','C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColIndex (ColInt 1)) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #5"
        quickCheck (tokeniseAndParse "GET Table('C1' AS 'users','C2')" == [S (Get NoDistinct (TName "Table") (Cols [Col (ColIndex (ColInt 1)) (Rename "users"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #6"
        quickCheck (tokeniseAndParse "GET Table('users','C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColIndex (ColName "users")) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #7"
        quickCheck (tokeniseAndParse "GET Table('users'*2,'C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColOp (BiArithOp Mul) (ColIndex (ColName "users")) (ColExpr (EArith (IntLit 2)))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #8"
        quickCheck (tokeniseAndParse "GET Table('users'+'cost','C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColOp (BiArithOp Add) (ColIndex (ColName "users")) (ColIndex (ColName "cost"))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #9"
        quickCheck (tokeniseAndParse "GET Table('users'+'cost' AS 'UsersCost','C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColOp (BiArithOp Add) (ColIndex (ColName "users")) (ColIndex (ColName "cost"))) (Rename "UsersCost"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #10"
        quickCheck (tokeniseAndParse "GET Table(('users'+'cost'),'C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColOp (BiArithOp Add) (ColIndex (ColName "users")) (ColIndex (ColName "cost"))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #11"
        quickCheck (tokeniseAndParse "GET Table(1234,'C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColExpr (EArith ( IntLit 1234))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #12"
        quickCheck (tokeniseAndParse "GET Table(1234.13,'C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColExpr (EArith ( FloatLit 1234.13))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #13"
        quickCheck (tokeniseAndParse "GET Table(1234.13 + 2*3,'C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColExpr (EArith ( ArithOp Add (EArith (FloatLit 1234.13)) (EArith (ArithOp Mul (EArith (IntLit 2)) (EArith (IntLit 3))))))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #14"
        quickCheck (tokeniseAndParse "GET Table(1234.13 + 2*3 AS 'Expr','C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColExpr (EArith ( ArithOp Add (EArith (FloatLit 1234.13)) (EArith (ArithOp Mul (EArith (IntLit 2)) (EArith (IntLit 3))))))) (Rename "Expr"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #15"
        quickCheck (tokeniseAndParse "GET Table(GET Table(*),'C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColExpr (EQuery ((Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #16"
        quickCheck (tokeniseAndParse "GET Table(GET Table(*) AS 'Query','C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColExpr (EQuery ((Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))))) (Rename "Query"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #17"     
        --quickCheck (tokeniseAndParse "GET Table(GET Table(*) + 'C2' / 2,'C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColOp (BiArithOp Add) (ColExpr (EArith ( ArithOp Add (EQuery (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))) (ColOp (BiArithOp Div) EArith (ArithOp Div (EArith (IntLit 2)) (EArith (IntLit 3))))))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #18"
        quickCheck (tokeniseAndParse "GET Table( SUM('C1'), COUNT('Users') )" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColAggregation Sum (ColInt 1)) NoRename, Col (ColAggregation Count (ColName "Users")) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #19"
        quickCheck (tokeniseAndParse "GET Table( SUM('C1') CONCAT COUNT('Users') )" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColOp (StrOp Concat) (ColAggregation Sum (ColInt 1)) (ColAggregation Count (ColName "Users"))) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #20"
        quickCheck (tokeniseAndParse "GET Table(*) UNION GET Table(*)" == [S (Union (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))])
        putStrLn "Test #21"
        quickCheck (tokeniseAndParse "GET Table(*) MERGE GET Table(*)" == [S (Merge (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))])
        putStrLn "Test #22"
        quickCheck (tokeniseAndParse "GET Table(*) UNION GET Table(*) UNION GET Table(*) " == [S (Union (Union (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))])
        putStrLn "Test #23"
        quickCheck (tokeniseAndParse "(GET Table(*) UNION GET Table(*)) UNION GET Table(*) " == [S (Union (Union (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))])
        putStrLn "Test #24"
        quickCheck (tokeniseAndParse "(GET Table(*) MERGE GET Table(*)) MERGE GET Table(*) " == [S (Merge (Merge (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))])
        putStrLn "Test #25"
        quickCheck (tokeniseAndParse "GET Table(*) MERGE GET Table(*) MERGE GET Table(*) " == [S (Merge (Merge (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))])
        putStrLn "Test #26"
        quickCheck (tokeniseAndParse "GET Table(GET Table(*) UNION GET Table(*) AS 'Query','C2')" == [ S (Get NoDistinct (TName "Table") (Cols [Col (ColExpr (EQuery ((Union (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))))) (Rename "Query"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #27"
        quickCheck (tokeniseAndParse "SUM (GET Table(GET Table(*) UNION GET Table(*) AS 'Query','C2'))" == [ S (AggreQuery Sum (Get NoDistinct (TName "Table") (Cols [Col (ColExpr (EQuery ((Union (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))))) (Rename "Query"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing)))])
        putStrLn "Test #28"
        quickCheck (tokeniseAndParse "COUNT (GET Table(*) MERGE GET Table(*) MERGE GET Table(*))" == [ S (AggreQuery Count (Merge (Merge (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing))))])
        putStrLn "Test #29"
        quickCheck (tokeniseAndParse "x UNION y" == [S (Union (VarQuery "x") (VarQuery "y"))])
        putStrLn "Test #30"
        quickCheck (tokeniseAndParse "x MERGE y" == [S (Merge (VarQuery "x") (VarQuery "y"))])
        putStrLn "Test #31"
        quickCheck (tokeniseAndParse "x UNION y UNION z" == [S (Union (Union (VarQuery "x") (VarQuery "y")) (VarQuery "z"))])
        putStrLn "Test #32"
        quickCheck (tokeniseAndParse "x MERGE y MERGE z" == [S (Merge (Merge (VarQuery "x") (VarQuery "y")) (VarQuery "z"))])
        putStrLn "Test #33"
        quickCheck (tokeniseAndParse "GET Table(x,y)" == [S (Get NoDistinct (TName "Table") (Cols [Col (ColExpr (EVar "x")) NoRename, Col (ColExpr (EVar "y")) NoRename]) (Nothing,Nothing))])
        putStrLn "Test #34"
        quickCheck (tokeniseAndParse "GET Table(x AS 'Users',y AS 'Cost')" == [S (Get NoDistinct (TName "Table") (Cols [Col (ColExpr (EVar "x")) (Rename "Users"), Col (ColExpr (EVar "y")) (Rename "Cost")]) (Nothing,Nothing))])
        --putStrLn "Test #35"
        --quickCheck (tokeniseAndParse "GET Table(*) INNER PRODUCT GET Table(*) GIVEN 'C1' != 'C2' " == [S (ProdQuery InnerProduct (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (ProdCondition Diff (ColIndex (ColInt 1)) (ColIndex (ColInt 2))))])
        --putStrLn "Test #36"
        --quickCheck (tokeniseAndParse "GET Table(*) FULL PRODUCT GET Table(*) GIVEN ('C1' != 'C2') && ('C2'= 'C3')" == [S (ProdQuery FullProduct (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (ProdBiCondition And (ProdCondition Diff (ColIndex (ColInt 1)) (ColIndex (ColInt 2))) (ProdCondition Eq (ColIndex (ColInt 2)) (ColIndex (ColInt 3)))))])
        --putStrLn "Test #37"
        --quickCheck (tokeniseAndParse "GET Table(*) FULL PRODUCT GET Table(*) GIVEN ('C1' != 'C2') && (('C2'*2) = 'C3')" == [S (ProdQuery FullProduct (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)) (ProdBiCondition And (ProdCondition Diff (ColIndex (ColInt 1)) (ColIndex (ColInt 2))) (ProdCondition Eq (ColOp (BiArithOp Mul) (ColIndex (ColInt 2)) (ColExpr (EArith (IntLit 2))) ) (ColIndex (ColInt 3)))))])
                

        putStrLn "Test #1"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE EMPTY('C1','Users')" == [S (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (Empty [ColInt 1, ColName "Users"])),Nothing))])
        putStrLn "Test #2"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE EMPTY('C1','Users') || BETWEEN('C2','C1','C3')" == [S (Get NoDistinct (TName "Table") AllColumns (Just (RowFilters Or (RowFilter (Empty [ColInt 1, ColName "Users"]))(RowFilter (RowBetw (ColIndex (ColInt 2)) (ColIndex (ColInt 1)) (ColIndex (ColInt 3))))),Nothing))])
        putStrLn "Test #3"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE 'C1'='C2'" == [S (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColIndex (ColInt 2)))),Nothing))])
        putStrLn "Test #4"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE 'C1'=2%3.10" == [S (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColExpr (EArith (ArithOp Mod (EArith (IntLit 2)) (EArith (FloatLit 3.10))))))),Nothing))])
        putStrLn "Test #5"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE 'C1'= GET Table(*)" == [S (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColExpr (EQuery (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))))),Nothing))])
        putStrLn "Test #6"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE 'C1'= (GET Table(*))" == [S (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColExpr (EQuery (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))))),Nothing))])
        putStrLn "Test #7"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE 'C1'= GET Table(*) WHERE EMPTY('C1','Users')" == [S (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColExpr (EQuery (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (Empty [ColInt 1, ColName "Users"])),Nothing)))))),Nothing))])
        putStrLn "Test #8"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE 'C1'= (GET Table(*) WHERE EMPTY('C1','Users'))" == [S (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColExpr (EQuery (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (Empty [ColInt 1, ColName "Users"])),Nothing)))))),Nothing))])
        putStrLn "Test #9"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE ('C1'= GET Table(*) WHERE EMPTY('C1','Users')) && 'C1'='C2'" == [S (Get NoDistinct (TName "Table") AllColumns (Just (RowFilters And (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColExpr (EQuery (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (Empty [ColInt 1, ColName "Users"])),Nothing)))))) (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColIndex (ColInt 2))))),Nothing))])
        putStrLn "Test #10"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE FIRST(1)" == [S (Get NoDistinct (TName "Table") AllColumns (Nothing,Just [First 1] ))])
        putStrLn "Test #11"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE FIRST(1), LAST(3), ORDERBY(ASC,'C1'), GROUPBY('C1')" == [S (Get NoDistinct (TName "Table") AllColumns (Nothing,Just [First 1, Last 3, (OrderBy Asc [ColIndex (ColInt 1)]), (GroupBy [ColIndex (ColInt 1)])] ))])
        putStrLn "Test #12"
        quickCheck (tokeniseAndParse "GET Table(*) WHERE ('C1'= GET Table(*) WHERE EMPTY('C1','Users')) && 'C1'='C2', FIRST(1), LAST(3), ORDERBY(ASC,'C1'), GROUPBY('C1')" == [S (Get NoDistinct (TName "Table") AllColumns (Just (RowFilters And (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColExpr (EQuery (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (Empty [ColInt 1, ColName "Users"])),Nothing)))))) (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColIndex (ColInt 2))))),Just [First 1, Last 3, (OrderBy Asc [ColIndex (ColInt 1)]), (GroupBy [ColIndex (ColInt 1)])] ))])
        
        
        --Tests for var assign
        putStrLn "Test #1"
        quickCheck (tokeniseAndParse "x := GET Table(*)" == [VarAssign "x" (EQuery (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))])
        putStrLn "Test #2"
        quickCheck (tokeniseAndParse "x := (GET Table(*))" == [VarAssign "x" (EQuery (Get NoDistinct (TName "Table") AllColumns (Nothing,Nothing)))])
        putStrLn "Test #3"
        quickCheck (tokeniseAndParse "x := x" == [VarAssign "x" (EVar "x")])
        putStrLn "Test #4"
        quickCheck (tokeniseAndParse "x := GET Table('C1','C2')" == [ VarAssign "x" (EQuery (Get NoDistinct (TName "Table") (Cols [Col (ColIndex (ColInt 1)) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing)))])
        putStrLn "Test #5"
        quickCheck (tokeniseAndParse "x := GET Table('C1' AS 'users','C2')" == [VarAssign "x" (EQuery(Get NoDistinct (TName "Table") (Cols [Col (ColIndex (ColInt 1)) (Rename "users"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing)))])
       
        -- The same as the ones above but with distinct
        putStrLn "Test #1"
        quickCheck (tokeniseAndParse "GET DISTINCT Table(*)" == [S (Get Distinct (TName "Table") AllColumns (Nothing,Nothing))])
        putStrLn "Test #2"
        quickCheck (tokeniseAndParse "GET DISTINCT Table('C1','C2')" == [ S (Get Distinct (TName "Table") (Cols [Col (ColIndex (ColInt 1)) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #3"
        quickCheck (tokeniseAndParse "GET DISTINCT Table('C1' AS 'users','C2')" == [S (Get Distinct (TName "Table") (Cols [Col (ColIndex (ColInt 1)) (Rename "users"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #4"
        quickCheck (tokeniseAndParse "GET DISTINCT Table('users','C2')" == [ S (Get Distinct (TName "Table") (Cols [Col (ColIndex (ColName "users")) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #5"
        quickCheck (tokeniseAndParse "GET DISTINCT Table('users'*2,'C2')" == [ S (Get Distinct (TName "Table") (Cols [Col (ColOp (BiArithOp Mul) (ColIndex (ColName "users")) (ColExpr (EArith (IntLit 2)))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #6"
        quickCheck (tokeniseAndParse "GET DISTINCT Table('users'+'cost','C2')" == [ S (Get Distinct (TName "Table") (Cols [Col (ColOp (BiArithOp Add) (ColIndex (ColName "users")) (ColIndex (ColName "cost"))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #7"
        quickCheck (tokeniseAndParse "GET DISTINCT Table('users'+'cost','C2')" == [ S (Get Distinct (TName "Table") (Cols [Col (ColOp (BiArithOp Add) (ColIndex (ColName "users")) (ColIndex (ColName "cost"))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #8"
        quickCheck (tokeniseAndParse "GET DISTINCT Table(*) WHERE ('C1'= GET Table(*) WHERE EMPTY('C1','Users')) && 'C1'='C2', FIRST(1), LAST(3), ORDERBY(ASC,'C1'), GROUPBY('C1')" == [S (Get Distinct (TName "Table") AllColumns (Just (RowFilters And (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColExpr (EQuery (Get NoDistinct (TName "Table") AllColumns (Just (RowFilter (Empty [ColInt 1, ColName "Users"])),Nothing)))))) (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColIndex (ColInt 2))))),Just [First 1, Last 3, (OrderBy Asc [ColIndex (ColInt 1)]), (GroupBy [ColIndex (ColInt 1)])] ))])


        -- The same as the ones above but with a query variable instead of Table
        putStrLn "Test #1"
        quickCheck (tokeniseAndParse "GET DISTINCT x(*)" == [S (Get Distinct (TQuery (VarQuery "x" )) AllColumns (Nothing,Nothing))])
        putStrLn "Test #2"
        quickCheck (tokeniseAndParse "GET DISTINCT x('C1','C2')" == [ S (Get Distinct (TQuery (VarQuery "x" )) (Cols [Col (ColIndex (ColInt 1)) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #3"
        quickCheck (tokeniseAndParse "GET DISTINCT x('C1' AS 'users','C2')" == [S (Get Distinct (TQuery (VarQuery "x" )) (Cols [Col (ColIndex (ColInt 1)) (Rename "users"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #4"
        quickCheck (tokeniseAndParse "GET DISTINCT x('users','C2')" == [ S (Get Distinct (TQuery (VarQuery "x" )) (Cols [Col (ColIndex (ColName "users")) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #5"
        quickCheck (tokeniseAndParse "GET DISTINCT x('users'*2,'C2')" == [ S (Get Distinct (TQuery (VarQuery "x" )) (Cols [Col (ColOp (BiArithOp Mul) (ColIndex (ColName "users")) (ColExpr (EArith (IntLit 2)))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #6"
        quickCheck (tokeniseAndParse "GET DISTINCT x('users'+'cost','C2')" == [ S (Get Distinct (TQuery (VarQuery "x" )) (Cols [Col (ColOp (BiArithOp Add) (ColIndex (ColName "users")) (ColIndex (ColName "cost"))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #7"
        quickCheck (tokeniseAndParse "GET DISTINCT x('users'+'cost','C2')" == [ S (Get Distinct (TQuery (VarQuery "x" )) (Cols [Col (ColOp (BiArithOp Add) (ColIndex (ColName "users")) (ColIndex (ColName "cost"))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #8"
        quickCheck (tokeniseAndParse "GET DISTINCT x(*) WHERE ('C1'= GET x(*) WHERE EMPTY('C1','Users')) && 'C1'='C2', FIRST(1), LAST(3), ORDERBY(ASC,'C1'), GROUPBY('C1')" == [S (Get Distinct (TQuery (VarQuery "x" )) AllColumns (Just (RowFilters And (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColExpr (EQuery (Get NoDistinct (TQuery (VarQuery "x" )) AllColumns (Just (RowFilter (Empty [ColInt 1, ColName "Users"])),Nothing)))))) (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColIndex (ColInt 2))))),Just [First 1, Last 3, (OrderBy Asc [ColIndex (ColInt 1)]), (GroupBy [ColIndex (ColInt 1)])] ))])

        -- The same as the ones above but with a query instead of a query variable
        putStrLn "Test #1"
        quickCheck (tokeniseAndParse "GET DISTINCT (GET DISTINCT Table(*))(*)" == [S (Get Distinct (TQuery (Get Distinct (TName "Table") AllColumns (Nothing,Nothing))) AllColumns (Nothing,Nothing))])
        putStrLn "Test #2"
        quickCheck (tokeniseAndParse "GET DISTINCT (GET DISTINCT (GET DISTINCT Table(*))(*))('C1','C2')" == [ S (Get Distinct (TQuery (Get Distinct (TQuery (Get Distinct (TName "Table") AllColumns (Nothing,Nothing))) AllColumns (Nothing,Nothing))) (Cols [Col (ColIndex (ColInt 1)) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #3"
        quickCheck (tokeniseAndParse "GET DISTINCT (GET DISTINCT (GET DISTINCT (GET DISTINCT Table(*))(*))('C1','C2'))('C1' AS 'users','C2')" == [S (Get Distinct (TQuery (Get Distinct (TQuery (Get Distinct (TQuery (Get Distinct (TName "Table") AllColumns (Nothing,Nothing))) AllColumns (Nothing,Nothing))) (Cols [Col (ColIndex (ColInt 1)) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))) (Cols [Col (ColIndex (ColInt 1)) (Rename "users"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #4"
        quickCheck (tokeniseAndParse "GET DISTINCT (GET DISTINCT (GET DISTINCT (GET DISTINCT (GET DISTINCT Table(*))(*))('C1','C2'))('C1' AS 'users','C2'))('users'*2,'C2')" == [ S (Get Distinct (TQuery (Get Distinct (TQuery (Get Distinct (TQuery (Get Distinct (TQuery (Get Distinct (TName "Table") AllColumns (Nothing,Nothing))) AllColumns (Nothing,Nothing))) (Cols [Col (ColIndex (ColInt 1)) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))) (Cols [Col (ColIndex (ColInt 1)) (Rename "users"), Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))) (Cols [Col (ColOp (BiArithOp Mul) (ColIndex (ColName "users")) (ColExpr (EArith (IntLit 2)))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #5"
        quickCheck (tokeniseAndParse "GET DISTINCT (GET DISTINCT Table(*))('users'*2,'C2')" == [ S (Get Distinct (TQuery (Get Distinct (TName "Table") AllColumns (Nothing,Nothing))) (Cols [Col (ColOp (BiArithOp Mul) (ColIndex (ColName "users")) (ColExpr (EArith (IntLit 2)))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #6"
        quickCheck (tokeniseAndParse "GET DISTINCT (GET DISTINCT Table(*))('users'+'cost','C2')" == [ S (Get Distinct (TQuery (Get Distinct (TName "Table") AllColumns (Nothing,Nothing))) (Cols [Col (ColOp (BiArithOp Add) (ColIndex (ColName "users")) (ColIndex (ColName "cost"))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #7"
        quickCheck (tokeniseAndParse "GET DISTINCT (GET DISTINCT Table(*))('users'+'cost','C2')" == [ S (Get Distinct (TQuery (Get Distinct (TName "Table") AllColumns (Nothing,Nothing))) (Cols [Col (ColOp (BiArithOp Add) (ColIndex (ColName "users")) (ColIndex (ColName "cost"))) NoRename, Col (ColIndex (ColInt 2)) NoRename])  (Nothing,Nothing))])
        putStrLn "Test #8"
        quickCheck (tokeniseAndParse "GET DISTINCT (GET DISTINCT Table(*))(*) WHERE ('C1'= GET (GET DISTINCT Table(*))(*) WHERE EMPTY('C1','Users')) && 'C1'='C2', FIRST(1), LAST(3), ORDERBY(ASC,'C1'), GROUPBY('C1')" == [S (Get Distinct (TQuery (Get Distinct (TName "Table") AllColumns (Nothing,Nothing))) AllColumns (Just (RowFilters And (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColExpr (EQuery (Get NoDistinct (TQuery (Get Distinct (TName "Table") AllColumns (Nothing,Nothing))) AllColumns (Just (RowFilter (Empty [ColInt 1, ColName "Users"])),Nothing)))))) (RowFilter (RowComparison Eq (ColIndex (ColInt 1))(ColIndex (ColInt 2))))),Just [First 1, Last 3, (OrderBy Asc [ColIndex (ColInt 1)]), (GroupBy [ColIndex (ColInt 1)])] ))])
        
        
        ------------------------------------------------------------ Tests for type checker--------------------------------------------------------------
        putStrLn "Tests for type Checker"
        putStrLn "Test #1"
        quickCheck (assertType "T" [String,String] [] "GET T(*)" [Table ["",""] [String,String]])
        putStrLn "Test #2"
        quickCheck (assertType "T" [Int,String,Bool] [] "GET T(*)" [Table ["","",""] [Int,String,Bool]])
        putStrLn "Test #3"
        quickCheck (assertType "T" [Int,String,Bool] ["'Id'","'Cost'","'Data'"] "GET T(*)" [Table ["Id","Cost","Data"] [Int,String,Bool]])
        putStrLn "Test #4"
        quickCheck (assertType "T" [String,String] [] "GET T('C1')" [Table [""] [String]])
        putStrLn "Test #5"
        quickCheck (assertType "T" [String,Int] [] "GET T('C2')" [Table [""] [Int]])
        putStrLn "Test #6"
        quickCheck (assertType "T" [String,Int] ["'Users'","'Cost'"] "GET T('Cost')" [Table ["Cost"] [Int]])
        putStrLn "Test #7"
        quickCheck (assertType "T" [String,Int] ["'Users'","'Cost'"] "GET T('Users','Cost')" [Table ["Users","Cost"] [String,Int]])
        putStrLn "Test #8"
        quickCheck (assertType "T" [String,Int] ["'Users'","'Cost'"] "GET T(12,12.10)" [Table ["",""] [Int,Float]])
        putStrLn "Test #9"
        quickCheck (assertType "T" [String,Int] ["'Users'","'Cost'"] "GET T(12,12.10)" [Table ["",""] [Int,Float]])
        putStrLn "Test #10"
        quickCheck (assertType "T" [String,Int] ["'Users'","'Cost'"] "GET T(\"hello\",12.10)" [Table ["",""] [String,Float]])
        putStrLn "Test #11"
        quickCheck (assertType "T" [String,Int] ["'Users'","'Cost'"] "GET T(\"hello\" AS 'Hello',12.10 AS 'Num')" [Table ["Hello","Num"] [String,Float]])
        putStrLn "Test #12"
        quickCheck (assertType "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T(\"hello\" CONCAT \"hello\" AS 'Hello',12.10 AS 'Num')" [Table ["Hello","Num"] [String,Float]])
        putStrLn "Test #13"
        quickCheck (assertType "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T('C1' CONCAT \"hello\", 'Cost', 'C3')" [Table ["","Cost","Id"] [String,Int,Int]])
        putStrLn "Test #14"
        quickCheck (assertType "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T('C1' CONCAT \"hello\" AS 'U', 'Cost' AS 'R', 'C3' AS 'T')" [Table ["U","R","T"] [String,Int,Int]])
        putStrLn "Test #15"
        quickCheck (assertType "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T('C1' CONCAT \"hello\" AS 'U', 'Cost'*1.0 AS 'R', 'C3'%2.3 AS 'T')" [Table ["U","R","T"] [String,Float,Float]])
        putStrLn "Test #16"
        quickCheck (assertType "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T('C1' CONCAT \"hello\" AS 'U', 'Cost'+'Id')" [Table ["U",""] [String,Int]])
        putStrLn "Test #17"
        quickCheck (assertType "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T('C1' CONCAT \"hello\" AS 'U', 'C2'+'C3')" [Table ["U",""] [String,Int]])
        putStrLn "Test #18"
        quickCheck (assertType "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T(True,False,True AND False OR True)" [Table ["","",""] [Bool,Bool,Bool]])
        putStrLn "Test #19"
        quickCheck (assertType "T" [String,Int] [] "GET T(\"hello\" AS 'Id','Id' CONCAT 'C1')" [Table ["Id",""] [String,String]])
        putStrLn "Test #20"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) AS R; GET T(GET R('C1'))" [Void,Table [""] [String]])
        putStrLn "Test #21"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET T(GET R('Id'))" [Void,Table [""] [String]])
        putStrLn "Test #22"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET DISTINCT T(GET R('Id'))" [Void,Table [""] [String]])
        putStrLn "Test #23"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R(*) UNION GET T(*)" [Void,Table ["Id","Cost"] [String,Int]])
        putStrLn "Test #24"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R('Id') UNION GET T('C1')" [Void,Table ["Id"] [String]])
        putStrLn "Test #25"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R(123) UNION GET T(45)" [Void,Table [""] [Int]])
        putStrLn "Test #26"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R(123/1.22) UNION GET T(45%1.10)" [Void,Table [""] [Float]])
        putStrLn "Test #27"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R(*) MERGE GET T(*)" [Void,Table ["Id","Cost"] [String,Int]])
        putStrLn "Test #28"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R('Id') MERGE GET T('C1')" [Void,Table ["Id"] [String]])
        putStrLn "Test #29"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R(123) MERGE GET T(45)" [Void,Table [""] [Int]])
        putStrLn "Test #30"
        quickCheck (assertType "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R(123/1.22) MERGE GET T(45%1.10)" [Void,Table [""] [Float]])
        putStrLn "Test #31"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R('Cost') MERGE GET T('C2')" [Void,Table ["Cost"] [Int]])
        putStrLn "Test #32"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; (GET R('Cost') WHERE (1*2+3) = 'C2')" [Void,Table ["Cost"] [Int]])
        putStrLn "Test #33"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET T('User') WHERE FIRST(10),ORDERBY(ASC,'User','C1','C2')" [Void,Table ["User"] [String]])
        putStrLn "Test #34"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "x := GET T(*); x" [Void,Table ["User","Cost"] [String,Int]])
        putStrLn "Test #35"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; x := (GET R('Cost') WHERE (1*2+3) = 'C2'); x" [Void,Void,Table ["Cost"] [Int]])
        putStrLn "Test #36"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "SUM(GET T('Cost'))" [Table ["Cost"] [Int]])
        putStrLn "Test #37"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "COUNT(GET T('C1'))" [Table ["User"] [Int]])
        putStrLn "Test #38"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "AVG(GET T('C2'))" [Table ["Cost"] [Float]])
        putStrLn "Test #39"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "GET T( AVG(GET T('C2')) AS 'Average' )" [Table ["Average"] [Float]])
        putStrLn "Test #40"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "MIN(GET T('C1'))" [Table ["User"] [String]])
        putStrLn "Test #41"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "GET T( MAX(GET T('C1')) AS 'Max' )" [Table ["Max"] [String]])
        putStrLn "Test #42"
        quickCheck (assertType "T" [String,Int] ["'User'","'Cost'"] "GET T( MAX(GET T('C1')) AS 'Max' )" [Table ["Max"] [String]])
        -- The products need to be done
       









        
        
        
        

        
        
        
        
        

        
        --The same as the ones above but with getconditions
        putStrLn "Test #1"
        quickCheck (assertType "T" [String,String] [] "GET T(*) WHERE 'C1'=\"Home\" || 'C2'=\"Hello\"" [Table ["",""] [String,String]])
        putStrLn "Test #2"
        quickCheck (assertType "T" [Int,String,Bool] [] "GET T(*) WHERE FIRST(1),LAST(2),GROUPBY('C1','C3')" [Table ["","",""] [Int,String,Bool]])
        putStrLn "Test #3"
        quickCheck (assertType "T" [Int,String,Bool] [] "GET T(*) WHERE BETWEEN('C1'*2,3,1.0),FIRST(1),LAST(2),GROUPBY('C1','C3')" [Table ["","",""] [Int,String,Bool]])
        putStrLn "Test #4"
        quickCheck (assertType "T" [String,String] [] "GET T('C1') WHERE ('C1' CONCAT 'C2') = \"Hello\"" [Table [""] [String]])
        putStrLn "Test #5"
        quickCheck (assertType "T" [String,Int] ["'Users'","'Cost'"] "GET T(\"hello\" AS 'Hello',12.10 AS 'Num') WHERE (('Num'+2) != 5.0) && (('Hello' CONCAT \"H\") <= \"P\")" [Table ["Hello","Num"] [String,Float]])
        
        --The same as the ones above but with type errors
       {- putStrLn "Test #1"
        quickCheck (expectTypeFailure "R" [String,String] [] "GET T(*)" [Table ["",""] [String,String]])
        putStrLn "Test #2"
        quickCheck (expectTypeFailure "T" [String,String] [] "GET T('C3')" [Table [""] [String]])
        putStrLn "Test #3"
        quickCheck (expectTypeFailure "T" [String,Int] [] "GET T('E')" [Table [""] [Int]])
        putStrLn "Test #4"
        quickCheck (expectTypeFailure "T" [String,Int] ["'Users'","'Cost'"] "GET T('Costs')" [Table ["Cost"] [Int]])
        putStrLn "Test #5"
        quickCheck (expectTypeFailure "T" [String,Int] ["'Users'","'Cost'"] "GET T('Users','Cost','C3')" [Table ["Users","Cost"] [String,Int]])
        putStrLn "Test #6"
        quickCheck (expectTypeFailure "T" [String,Int] ["'Users'","'Cost'"] "GET T(12+'C1',12.10)" [Table ["",""] [Int,Float]])
        putStrLn "Test #7"
        quickCheck (expectTypeFailure "T" [String,Int] ["'Users'","'Cost'"] "GET T(12,12.10/'Users')" [Table ["",""] [Int,Float]])
        putStrLn "Test #8"
        quickCheck (expectTypeFailure "T" [String,Int] ["'Users'","'Cost'"] "GET T(\"hello\" CONCAT 4,12.10)" [Table ["",""] [String,Float]])
        putStrLn "Test #9"
        quickCheck (expectTypeFailure "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T('C1' CONCAT \"hello\" + 4, 'Cost', 'C3')" [Table ["","Cost","Id"] [String,Int,Int]])
        putStrLn "Test #10"
        quickCheck (expectTypeFailure "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T('C1' CONCAT \"hello\" AS 'U', 'Cost' AS 'R', 'C3' AS 'T','Socks')" [Table ["U","R","T"] [String,Int,Int]])
        putStrLn "Test #11"
        quickCheck (expectTypeFailure "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T('C1' CONCAT \"hello\" AS 'U', 'Users'*1.0 AS 'R', 'C3'%2.3 AS 'T')" [Table ["U","R","T"] [String,Float,Float]])
        putStrLn "Test #12"
        quickCheck (expectTypeFailure "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T('C1' CONCAT \"hello\" AS 'U', 'Cost'+'Id'-\"Pizza\")" [Table ["U",""] [String,Int]])
        putStrLn "Test #13"
        quickCheck (expectTypeFailure "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T('C1' CONCAT \"hello\" AS 'U', ('C2'+'C3') < True)" [Table ["U",""] [String,Int]])
        putStrLn "Test #14"
        quickCheck (expectTypeFailure "T" [String,Int,Int] ["'Users'","'Cost'","'Id'"] "GET T(True,False,True AND False OR True AND 'C1')" [Table ["","",""] [Bool,Bool,Bool]])
        putStrLn "Test #15"
        quickCheck (expectTypeFailure "T" [String,Int] [] "GET T(\"hello\" AS 'Id','Id' CONCAT 'C1' < 4)" [Table ["Id",""] [String,Bool]])
        putStrLn "Test #16"
        quickCheck (expectTypeFailure "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) AS R; GET T(GET R('C3'))" [Void,Table [""] [String]])
        putStrLn "Test #17"
        quickCheck (expectTypeFailure "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET T(GET R('Id','Cost'))" [Void,Table [""] [String]])
        putStrLn "Test #18"
        quickCheck (expectTypeFailure "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R('Id') UNION GET T(*)" [Void,Table ["Id","Cost"] [String,Int]])
        putStrLn "Test #19"
        quickCheck (expectTypeFailure "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R('Cost') UNION GET T('C1')" [Void,Table ["Id"] [String]])
        putStrLn "Test #20"
        quickCheck (expectTypeFailure "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R(123.0) UNION GET T(45)" [Void,Table [""] [Int]])
        putStrLn "Test #21"
        quickCheck (expectTypeFailure "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R(123/1.22) UNION GET T(45%1)" [Void,Table [""] [Float]])
        putStrLn "Test #22"
        quickCheck (expectTypeFailure "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R(\"Hello\") MERGE GET T(*)" [Void,Table ["Id","Cost"] [String,Int]])
        putStrLn "Test #23"
        quickCheck (expectTypeFailure "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int,Bool) ('Id','Cost') AS R; GET R('C3') MERGE GET T('C1')" [Void,Table ["Id"] [String]])
        putStrLn "Test #24"
        quickCheck (expectTypeFailure "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R(True) MERGE GET T(45)" [Void,Table [""] [Int]])
        putStrLn "Test #25"
        quickCheck (expectTypeFailure "T" [String,Int] [] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R('Cost'*2/10+3) MERGE GET T(45%1.10)" [Void,Table [""] [Float]])
        putStrLn "Test #26"
        quickCheck (expectTypeFailure "T" [String,String] [] "GET T(*) WHERE 'C1'=2 || 'C2'=\"Hello\"" [Table ["",""] [String,String]])
        putStrLn "Test #27"
        quickCheck (expectTypeFailure "T" [Int,String,Bool] [] "GET T(*) WHERE FIRST(1),LAST(2),GROUPBY('C1','C0')" [Table ["","",""] [Int,String,Bool]])
        putStrLn "Test #28"
        quickCheck (expectTypeFailure "T" [Int,String,Bool] [] "GET T(*) WHERE BETWEEN('C1'*2,3,\"Hello\"),FIRST(1),LAST(2),GROUPBY('C1','C3')" [Table ["","",""] [Int,String,Bool]])
        putStrLn "Test #29"
        quickCheck (expectTypeFailure "T" [String,String] [] "GET T('C1') WHERE ('C1' CONCAT 'C2') = 12.0" [Table [""] [String]])
        putStrLn "Test #30"
        quickCheck (expectTypeFailure "T" [String,Int] ["'Users'","'Cost'"] "GET T(\"hello\" AS 'Hello',12.10 AS 'Num') WHERE (('Num'+2) != 5.0) && (('Hello' CONCAT \"H\") <= True)" [Table ["Hello","Num"] [String,Float]])
        putStrLn "Test #31"        
        quickCheck (expectTypeFailure "T" [String,Int] ["'User'","'Cost'"] "IMPORT 'hello.csv' :: (String,Int) ('Id','Cost') AS R; GET R('Cost') WHERE 'C2'= 1*2+3 MERGE GET T('User') WHERE FIRST(10),ORDERBY(ASC,'User',\"Peace\",\"Cranberries\")" [Void,Table [""] [Float]])
        putStrLn "Test #32"
        quickCheck (expectTypeFailure "T" [String,Int] ["'User'","'Cost'"] "SUM(GET T('User'))" [Table ["Cost"] [Int]])
        putStrLn "Test #33"
        quickCheck (expectTypeFailure "T" [String,Int] ["'User'","'Cost'"] "AVG(GET T('C1'))" [Table ["Cost"] [Float]])
        putStrLn "Test #34"
        quickCheck (expectTypeFailure "T" [String,Int] ["'User'","'Cost'"] "GET T( AVG(GET T('User')) AS 'Average' )" [Table ["Average"] [Float]]) -}
        let impressionFilePath = "C:\\Users\\tamaj\\OneDrive\\TestsSQL-Like\\impression.csv"
        let impressionFilePathLarge = "C:\\Users\\tamaj\\OneDrive\\TestsSQL-Like\\impressionLarge.csv"
        let impressionEmptyFilePath = "C:\\Users\\tamaj\\OneDrive\\TestsSQL-Like\\impressionEmpty.csv"
        let testDir = "C:\\Users\\tamaj\\OneDrive\\TestsSQL-Like\\"   
        putStrLn "Tests with real csv files"
        putStrLn "Test #1"
        quickCheck (assertQueryResult (testDir++"test1.csv") impressionFilePath "T" [String,String,String,String,String,String,String] [] "GET T(*)")
        putStrLn "Test #2"
        quickCheck (assertQueryResult (testDir++"test2.csv") impressionFilePath "T" [String,String,String,String,String,String,String] [] "GET T('C1')")
        putStrLn "Test #3"
        quickCheck (assertQueryResult (testDir++"test3.csv") impressionFilePath "T" [String,String,String,String,String,String,String] [] "GET T('C3')")
        putStrLn "Test #4"
        quickCheck (assertQueryResult (testDir++"test4.csv") impressionFilePath "T" [String,String,String,String,String,String,String] [] "GET T('C1','C3')")
        putStrLn "Test #5"
        quickCheck (assertQueryResult (testDir++"test5.csv") impressionFilePath "T" [String,String,String,String,String,String,String] [] "GET T('C3','C1')")
        putStrLn "Test #6"
        quickCheck (assertQueryResult (testDir++"test1.csv") impressionFilePath "T" [String,String,String,String,String,String,String] [] "GET T('C1','C2','C3','C4','C5','C6','C7')")
        putStrLn "Test #7"
        quickCheck (assertQueryResult (testDir++"test1.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Date','Id','Gender','Age','Income','Context','Cost')")
        putStrLn "Test #8"
        quickCheck (assertQueryResult (testDir++"test2.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Date')")
        putStrLn "Test #9"
        quickCheck (assertQueryResult (testDir++"test3.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender')")
        putStrLn "Test #10"
        quickCheck (assertQueryResult (testDir++"test4.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Date','Gender')")
        putStrLn "Test #11"
        quickCheck (assertQueryResult (testDir++"test5.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender','Date')")
        putStrLn "Test #12"
        quickCheck (assertQueryResult (testDir++"test6.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(1,'Gender','Date')")
        putStrLn "Test #13"
        quickCheck (assertQueryResult (testDir++"test6.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(1,'C3','C1')")
        putStrLn "Test #14"
        quickCheck (assertQueryResult (testDir++"test7.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(\"hello\",'C3','C1')")
        putStrLn "Test #15"
        quickCheck (assertQueryResult (testDir++"test7.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(\"hello\",'Gender','Date')")
        putStrLn "Test #16"
        quickCheck (assertQueryResult (testDir++"test8.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(\"hello\",1,\"benny\")")
        putStrLn "Test #17"
        quickCheck (assertQueryResult (testDir++"test9.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(2*3,\"hello\" CONCAT \"hello\")")
        putStrLn "Test #18"
        quickCheck (assertQueryResult (testDir++"test10.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender' CONCAT \"Male\")")
        putStrLn "Test #19"
        quickCheck (assertQueryResult (testDir++"test10.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C3' CONCAT \"Male\")")
        putStrLn "Test #20"
        quickCheck (assertQueryResult (testDir++"test11.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(\"Male\" CONCAT 'C3')")
        putStrLn "Test #21"
        quickCheck (assertQueryResult (testDir++"test11.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(\"Male\" CONCAT 'Gender')")
        putStrLn "Test #22"
        quickCheck (assertQueryResult (testDir++"test1.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*))(*)")
        putStrLn "Test #23"
        quickCheck (assertQueryResult (testDir++"test2.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('C1'))(*)")
        putStrLn "Test #24"
        quickCheck (assertQueryResult (testDir++"test2.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('C1'))('C1')")
        putStrLn "Test #25"
        quickCheck (assertQueryResult (testDir++"test2.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('C1'))('Date')")
        putStrLn "Test #26"
        quickCheck (assertQueryResult (testDir++"test2.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('C1' AS 'R'))('R')")
        putStrLn "Test #27"
        quickCheck (assertQueryResult (testDir++"test7.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(\"hello\",'C3','C1'))(*)")
        putStrLn "Test #28"
        quickCheck (assertQueryResult (testDir++"test7.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(\"hello\",'C3','C1'))('C1','C2','C3')")
        putStrLn "Test #29"
        quickCheck (assertQueryResult (testDir++"test7.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(\"hello\" AS 'T','C3' AS 'R','C1' AS 'Q'))('T','R','Q')")
        putStrLn "Test #30"
        quickCheck (assertQueryResult (testDir++"test7.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(\"hello\" AS 'T','C3' AS 'R','C1' AS 'Q'))('T','C2','Q')")
        putStrLn "Test #31"
        quickCheck (assertQueryResult (testDir++"test12.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "COUNT(GET T(*))")
        putStrLn "Test #32"
        quickCheck (assertQueryResult (testDir++"test13.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "MIN(GET T('C3'))")
        putStrLn "Test #33"
        quickCheck (assertQueryResult (testDir++"test13.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "MIN(GET T('Gender'))")
        putStrLn "Test #34"
        quickCheck (assertQueryResult (testDir++"test14.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "MAX(GET T('Gender'))")
        putStrLn "Test #35"
        quickCheck (assertQueryResult (testDir++"test14.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "MAX(GET T('C3'))")
        putStrLn "Test #36"
        quickCheck (assertQueryResult (testDir++"test15.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'Gender'=\"Female\"")
        putStrLn "Test #37"
        quickCheck (assertQueryResult (testDir++"test15.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'C3'=\"Female\"")
        putStrLn "Test #38"
        quickCheck (assertQueryResult (testDir++"test16.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'Id'= \"8988131206684964864\"")        
        putStrLn "Test #39"
        quickCheck (assertQueryResult (testDir++"test16.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'C2'= \"8988131206684964864\"")
        putStrLn "Test #40"
        quickCheck (assertQueryResult (testDir++"test17.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Age') WHERE 'Gender'=\"Female\"")
        putStrLn "Test #41"
        quickCheck (assertQueryResult (testDir++"test17.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C4') WHERE 'Gender'=\"Female\"")
        putStrLn "Test #42"
        quickCheck (assertQueryResult (testDir++"test17.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C4') WHERE 'C3'=\"Female\"")
        putStrLn "Test #43"
        quickCheck (assertQueryResult (testDir++"test18.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Age') WHERE 'Id'= \"8988131206684964864\"")        
        putStrLn "Test #44"
        quickCheck (assertQueryResult (testDir++"test18.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C4') WHERE 'Id'= \"8988131206684964864\"")
        putStrLn "Test #45"
        quickCheck (assertQueryResult (testDir++"test18.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C4') WHERE 'C2'= \"8988131206684964864\"")
        putStrLn "Test #46"
        quickCheck (assertQueryResult (testDir++"test19.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Date') WHERE 'Gender' = \"Female\" && 'Age' = \"25-34\"")
        putStrLn "Test #47"
        quickCheck (assertQueryResult (testDir++"test19.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C1') WHERE 'Gender' = \"Female\" && 'C4' = \"25-34\"")
        putStrLn "Test #48"
        quickCheck (assertQueryResult (testDir++"test20.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Context','Cost') WHERE 'Gender' = \"Female\" || 'C4' = \"25-34\"")
        putStrLn "Test #49"
        quickCheck (assertQueryResult (testDir++"test20.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C6','C7') WHERE 'C3' = \"Female\" || 'Age' = \"25-34\"")
        putStrLn "Test #50"
        quickCheck (assertQueryResult (testDir++"test21.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE ('C3' = \"Female\" && 'Age' = \"25-34\") || 'Context' = \"News\"")
        putStrLn "Test #51"
        quickCheck (assertQueryResult (testDir++"test22.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'C3' = \"Female\" || 'Age' = \"25-34\"")
        putStrLn "Test #52"
        quickCheck (assertQueryResult (testDir++"test23.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'Context' = \"News\" || 'Context' = \"Shopping\"")
        putStrLn "Test #53"
        quickCheck (assertQueryResult (testDir++"test24.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Id') WHERE 'Context' = \"News\" || 'Gender' = \"Female\"")
        putStrLn "Test #54"
        quickCheck (assertQueryResult (testDir++"test24.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C2') WHERE 'Context' = \"News\" || 'C3' = \"Female\"")
        putStrLn "Test #55"
        quickCheck (assertQueryResult (testDir++"test25.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Context') WHERE 'Income' = \"High\" || 'Gender' = \"Female\" || 'Context' = \"Blog\"") 
        putStrLn "Test #56"
        quickCheck (assertQueryResult (testDir++"test25.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Context') WHERE ('Gender' = \"Female\") || ('Context' = \"Blog\") || ('Income' = \"High\")")
        putStrLn "Test #57"
        quickCheck (assertQueryResult (testDir++"test26.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Context') WHERE ('Income' = \"High\" || 'Gender' = \"Female\") && 'Context' = \"Blog\"")
        putStrLn "Test #58"
        quickCheck (assertQueryResult (testDir++"test26.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Context') WHERE ('Income' = \"High\" || 'Gender' = \"Female\") && 'Context' = \"Blog\"")
        putStrLn "Test #59"        
        quickCheck (assertQueryResult (testDir++"test27.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET DISTINCT T('Date') WHERE 'Gender' = \"Female\" && 'Age' = \"25-34\"")
        putStrLn "Test #60"
        quickCheck (assertQueryResult (testDir++"test28.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET DISTINCT T('Context','Cost') WHERE 'Gender' = \"Female\" || 'C4' = \"25-34\"")
        putStrLn "Test #61"
        quickCheck (assertQueryResult (testDir++"test29.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET DISTINCT T(*) WHERE ('C3' = \"Female\" && 'Age' = \"25-34\") || 'Context' = \"News\"")
        putStrLn "Test #62"
        quickCheck (assertQueryResult (testDir++"test30.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET DISTINCT T('Context') WHERE ('Gender' = \"Female\") || ('Context' = \"Blog\") || ('Income' = \"High\")")
        putStrLn "Test #63"
        quickCheck (assertQueryResult (testDir++"test31.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Id') WHERE 'Context' = \"News\" || 'Gender' = \"Female\",FIRST(10)")
        putStrLn "Test #64"
        quickCheck (assertQueryResult (testDir++"test32.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET DISTINCT T('Date') WHERE 'Gender' = \"Female\" && 'Age' = \"25-34\",FIRST(32)")
        putStrLn "Test #65"
        quickCheck (assertQueryResult (testDir++"test33.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET DISTINCT T('Context') WHERE ('Income' = \"High\" || 'Gender' = \"Female\") && 'Context' = \"Blog\",FIRST(4)")
        putStrLn "Test #66" 
        quickCheck (assertQueryResult (testDir++"test34.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE ORDERBY(ASC,'Gender')")
        putStrLn "Test #67"
        quickCheck (assertQueryResult (testDir++"test34.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE ORDERBY(ASC,'C3')")
        putStrLn "Test #68"
        quickCheck (assertQueryResult (testDir++"test35.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE ORDERBY(ASC,'Date','Context','Gender')")
        putStrLn "Test #69"
        quickCheck (assertQueryResult (testDir++"test35.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE ORDERBY(ASC,'C1','C6','C3')")
        putStrLn "Test #70"
        quickCheck (assertQueryResult (testDir++"test36.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C1') WHERE ORDERBY(DESC,'C1')")
        putStrLn "Test #71"
        quickCheck (assertQueryResult (testDir++"test37.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C1','Id') WHERE ORDERBY(DESC,'Date','Id')")
        putStrLn "Test #72"
        quickCheck (assertQueryResult (testDir++"test38.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] " COUNT( GET T('Gender') WHERE 'Gender' = \"Male\") UNION COUNT( GET T('Gender') )")
        putStrLn "Test #73"
        quickCheck (assertQueryResult (testDir++"test39.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] " GET (GET T('Date') UNION GET T('Gender'))(*) WHERE ORDERBY(ASC,'C1') ")
        putStrLn "Test #74"
        quickCheck (assertQueryResult (testDir++"test39.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('C1') UNION GET T('C3'))(*) WHERE ORDERBY(ASC,'C1') ")
        putStrLn "Test #75"
        quickCheck (assertQueryResult (testDir++"test40.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('C1') MERGE GET T('C3'))(*) WHERE ORDERBY(ASC,'C1') ")
        putStrLn "Test #76"
        quickCheck (assertQueryResult (testDir++"test40.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('Date') MERGE GET T('Gender'))(*) WHERE ORDERBY(ASC,'C1') ")
        putStrLn "Test #77"
        quickCheck (assertQueryResult (testDir++"test41.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(COUNT('Gender'),'Gender') WHERE GROUPBY('Gender')")
        putStrLn "Test #78"
        quickCheck (assertQueryResult (testDir++"test42.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('Date',COUNT('Date')) WHERE GROUPBY('Date'))(*) WHERE ORDERBY(ASC,'C1')")
        putStrLn "Test #79"
        quickCheck (assertQueryResult (testDir++"test42.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('Date',COUNT('Date')) WHERE GROUPBY('Date'))('C1','C2') WHERE ORDERBY(ASC,'Date')")
        putStrLn "Test #80"
        quickCheck (assertQueryResult (testDir++"test43.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('Context',MIN('Date')) WHERE GROUPBY('Context'))(*) WHERE ORDERBY(DESC,'Context')")
        putStrLn "Test #81"
        quickCheck (assertQueryResult (testDir++"test43.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('Context',MIN('Date')) WHERE GROUPBY('Context'))('C1','C2') WHERE ORDERBY(DESC,'Context')")
        putStrLn "Test #82"
        quickCheck (assertQueryResult (testDir++"test44.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('Age',MAX('Cost')) WHERE GROUPBY('Age'))(*) WHERE ORDERBY(ASC,'Age')")
        putStrLn "Test #83"
        quickCheck (assertQueryResult (testDir++"test45.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('Id',COUNT('Id')) WHERE GROUPBY('Id'))(*) WHERE ORDERBY(ASC,'Id')")
        putStrLn "Test #84"
        quickCheck (assertQueryResult (testDir++"test46.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('Income', 'Context', MAX('Cost')) WHERE GROUPBY('Income','Context'))(*) WHERE ORDERBY(ASC,'Income','Context')")
        putStrLn "Test #85"
        quickCheck (assertQueryResult (testDir++"test46.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('Income', 'Context', MAX('Cost')) WHERE GROUPBY('Income','Context'))(*) WHERE ORDERBY(ASC,'Income','Context')")
        putStrLn "Test #86"
        quickCheck (assertQueryResult (testDir++"test47.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) WHERE 'Date' = MAX(GET T('Date')))(*) WHERE ORDERBY(ASC,'Gender')")
        putStrLn "Test #87"
        quickCheck (assertQueryResult (testDir++"test48.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('Gender',COUNT('Gender')) WHERE 'Income' = \"High\", GROUPBY('Gender'))(*) WHERE ORDERBY(ASC,'Gender')")
        putStrLn "Test #88"
        quickCheck (assertQueryResult (testDir++"test48.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender',COUNT('Gender')) WHERE 'Income' = \"High\", GROUPBY('Gender'),ORDERBY(ASC,'Gender')")
        putStrLn "Test #89"
        quickCheck (assertQueryResult (testDir++"test49.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender',MIN('Date'),\"hello\" CONCAT \" everyone\") WHERE 'Income' = \"High\", GROUPBY('Gender'),ORDERBY(ASC,'Gender'),FIRST(23)")
        putStrLn "Test #90"
        quickCheck (assertQueryResult (testDir++"test50.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'Context' SATISFIES = (\"Blog\",\"News\",\"Shopping\")")
        putStrLn "Test #91"
        quickCheck (assertQueryResult (testDir++"test51.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'Gender' SATISFIES < (MAX(GET T('Gender')))")
        putStrLn "Test #92"
        quickCheck (assertQueryResult (testDir++"test51.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'Gender' < MAX(GET T('Gender'))")
        putStrLn "Test #93"
        quickCheck (assertQueryResult (testDir++"test52.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'Gender' > MIN(GET T('Gender'))")
        putStrLn "Test #94"
        quickCheck (assertQueryResult (testDir++"test52.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE 'Gender' SATISFIES > (MIN(GET T('Gender')))")
        putStrLn "Test #95"
        quickCheck (assertQueryResult (testDir++"test53.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE BETWEEN('Date',\"2015-01-01 12:00:02\",\"2015-01-01 12:00:22\")")
        putStrLn "Test #96"     
        quickCheck (assertQueryResult (testDir++"test54.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(*) WHERE NOTBETWEEN('Date',\"2015-01-01 12:00:02\",\"2015-01-01 12:00:22\")")
        putStrLn "Test #97" 
        quickCheck (assertQueryResult (testDir++"test55.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender',COUNT('Gender')) WHERE 'Context' SATISFIES = (\"Blog\",\"News\",\"Shopping\"), GROUPBY('Gender'),ORDERBY(ASC,'Gender')")
        putStrLn "Test #98"
        quickCheck (assertQueryResult (testDir++"test56.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender',COUNT('Gender')) WHERE 'Context' SATISFIES = (\"Blog\",\"News\",\"Shopping\"), GROUPBY('Gender'),ORDERBY(ASC,'Gender'),FIRST(3)")
        putStrLn "Test #99"
        quickCheck (assertQueryResult (testDir++"test57.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender',COUNT('Gender')) WHERE 'Context' SATISFIES = (\"Blog\",\"News\",\"Shopping\"), GROUPBY('Gender'),ORDERBY(DESC,'Gender'),FIRST(3)")
        putStrLn "Test #100"
        quickCheck (assertQueryResult (testDir++"test57.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender',COUNT('Gender')) WHERE 'Context' = \"Blog\" || 'Context' = \"News\" || 'Context' = \"Shopping\", GROUPBY('Gender'),ORDERBY(DESC,'Gender'),FIRST(3)")
        putStrLn "Test #101"
        quickCheck (assertQueryResult (testDir++"test58.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) INNER PRODUCT GET T(*) GIVEN 'C1' = 'C8')(*) WHERE ORDERBY(ASC,'Date','Id','C9')")
        putStrLn "Test #102"
        quickCheck (assertQueryResult (testDir++"test58.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) PRODUCT GET T(*) GIVEN 'C1' = 'C8')(*) WHERE ORDERBY(ASC,'Date','Id','C9')")
        putStrLn "Test #103"
        quickCheck (assertQueryResult (testDir++"test59.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) INNER PRODUCT GET T(*) GIVEN True = True)(*) WHERE ORDERBY(ASC,'Date','Id','C8','C9')")
        putStrLn "Test #104"
        quickCheck (assertQueryResult (testDir++"test59.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) PRODUCT GET T(*) GIVEN True = True)(*) WHERE ORDERBY(ASC,'Date','Id','C8','C9')")
        putStrLn "Test #105"
        quickCheck (assertQueryResult (testDir++"test60.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) INNER PRODUCT GET T(*) GIVEN 'C1' < 'C8')(*) WHERE ORDERBY(ASC,'Date','Id','C9')")
        putStrLn "Test #106"
        quickCheck (assertQueryResult (testDir++"test60.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) PRODUCT GET T(*) GIVEN 'C1' < 'C8')(*) WHERE ORDERBY(ASC,'Date','Id','C9')")
        putStrLn "Test #107"
        quickCheck (assertQueryResult (testDir++"test61.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) INNER PRODUCT GET T(*) GIVEN 'C1' < 'C8' || 'Gender' = 'C10')(*) WHERE ORDERBY(ASC,'Date','Id','C9')")
        putStrLn "Test #108"
        quickCheck (assertQueryResult (testDir++"test61.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) PRODUCT GET T(*) GIVEN 'C1' < 'C8' || 'Gender' = 'C10')(*) WHERE ORDERBY(ASC,'Date','Id','C9')")
        putStrLn "Test #109"
        quickCheck (assertQueryResult (testDir++"test62.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) LEFT PRODUCT GET T(*) GIVEN 'C1' = 'C8' && 'C2' != 'C9')(*) WHERE ORDERBY(ASC,'Date','Id','C9')")
        putStrLn "Test #110"
        quickCheck (assertQueryResult (testDir++"test63.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*) LEFT PRODUCT GET T(*) GIVEN 'C7' = 'C14' && 'C2' != 'C9')('C7','C14') WHERE ORDERBY(ASC,'C7','C14')")
        putStrLn "Test #111"
        quickCheck (assertQueryResult (testDir++"test64.csv") impressionEmptyFilePath "T" [String,String,String,String,String,String,String,String,String,String,String,String,String,String] ["'Date1'","'Id1'","'Gender1'","'Age1'","'Income1'","'Context1'","'Cost1'","'Date2'","'Id2'","'Gender2'","'Age2'","'Income2'","'Context2'","'Cost2'"] "GET T(*) WHERE EMPTY('Date2')")
        putStrLn "Test #112"
        quickCheck (assertQueryResult (testDir++"test64.csv") impressionEmptyFilePath "T" [String,String,String,String,String,String,String,String,String,String,String,String,String,String] ["'Date1'","'Id1'","'Gender1'","'Age1'","'Income1'","'Context1'","'Cost1'","'Date2'","'Id2'","'Gender2'","'Age2'","'Income2'","'Context2'","'Cost2'"] "GET T(*) WHERE EMPTY('C8')")
        putStrLn "Test #113"
        quickCheck (assertQueryResult (testDir++"test65.csv") impressionEmptyFilePath "T" [String,String,String,String,String,String,String,String,String,String,String,String,String,String] ["'Date1'","'Id1'","'Gender1'","'Age1'","'Income1'","'Context1'","'Cost1'","'Date2'","'Id2'","'Gender2'","'Age2'","'Income2'","'Context2'","'Cost2'"] "GET T(*) WHERE NOTEMPTY('C8')")
        putStrLn "Test #114"
        quickCheck (assertQueryResult (testDir++"test65.csv") impressionEmptyFilePath "T" [String,String,String,String,String,String,String,String,String,String,String,String,String,String] ["'Date1'","'Id1'","'Gender1'","'Age1'","'Income1'","'Context1'","'Cost1'","'Date2'","'Id2'","'Gender2'","'Age2'","'Income2'","'Context2'","'Cost2'"] "GET T(*) WHERE NOTEMPTY('Date2')")
        putStrLn "Test #115"
        quickCheck (assertQueryResult (testDir++"test7.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "x := \"hello\"; GET T(x,'C3','C1')")
        putStrLn "Test #116"
        quickCheck (assertQueryResult (testDir++"test8.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "x := \"hello\"; y := 1; z := \"benny\"; GET T(x,y,z)")
        putStrLn "Test #117"
        quickCheck (assertQueryResult (testDir++"test9.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "calc := 2*3; hello := \"hello\"; GET T(calc, hello CONCAT hello)")
        putStrLn "Test #118"
        quickCheck (assertQueryResult (testDir++"test10.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "m := \"Male\"; GET T('C3' CONCAT m)")
        putStrLn "Test #119"
        quickCheck (assertQueryResult (testDir++"test7.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "hello := \"hello\"; GET (GET T(hello AS 'T','C3' AS 'R','C1' AS 'Q'))('T','R','Q')")
        putStrLn "Test #120"
        quickCheck (assertQueryResult (testDir++"test16.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "id := \"8988131206684964864\"; GET T(*) WHERE 'Id'= id")
        putStrLn "Test #121"
        quickCheck (assertQueryResult (testDir++"test19.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "gender := \"Female\"; age := \"25-34\"; GET T('Date') WHERE 'Gender' = gender && 'Age' = age")
        putStrLn "Test #122"
        quickCheck (assertQueryResult (testDir++"test27.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "age := \"25-34\"; GET DISTINCT T('Date') WHERE 'Gender' = \"Female\" && 'Age' = age")
        putStrLn "Test #123"
        quickCheck (assertQueryResult (testDir++"test38.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] " gender := \"Male\"; q1 := GET T('Gender') WHERE 'Gender' = gender; q2 := GET T('Gender'); COUNT(q1) UNION COUNT(q2)")
        putStrLn "Test #124"
        quickCheck (assertQueryResult (testDir++"test46.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "q := GET T('Income', 'Context', MAX('Cost')) WHERE GROUPBY('Income','Context'); GET q(*) WHERE ORDERBY(ASC,'Income','Context')")
        putStrLn "Test #125"
        quickCheck (assertQueryResult (testDir++"test51.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] " max := MAX(GET T('Gender')); GET T(*) WHERE 'Gender' < max")
        putStrLn "Test #126"
        quickCheck (assertQueryResult (testDir++"test55.csv") impressionFilePath "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "blog := \"Blog\"; shopping := \"Shopping\"; GET T('Gender',COUNT('Gender')) WHERE 'Context' SATISFIES = (blog,\"News\",shopping), GROUPBY('Gender'),ORDERBY(ASC,'Gender')")
        
        --- TESTS FOR PROBLEM 1
        let testDirProblem1 = "C:\\Users\\tamaj\\OneDrive\\TestsSQL-Like\\problem1\\" 
        putStrLn "Test #1"
        quickCheck (assertQueryResult (testDirProblem1++"test1.csv") (testDirProblem1 ++ "problem1.csv") "T" [String,String] ["'Income'","'Context'"] ("IMPORT '" ++ ((testDirProblem1 ++ "problem11.csv")) ++ "' :: (String,String) ('Income1','Context1') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))
        putStrLn "Test #2"
        quickCheck (assertQueryResult (testDirProblem1++"test1.csv") (testDirProblem1 ++ "problem1.csv") "T" [String,String] ["'Income'","'Context'"] ("IMPORT '" ++ ((testDirProblem1 ++ "problem11.csv")) ++ "' :: (String,String) ('Income1','Context1') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'Income','Context','Income1','Context1')"))
        putStrLn "Test #3"
        quickCheck (assertQueryResult (testDirProblem1++"test1.csv") (testDirProblem1 ++ "problem1.csv") "T" [String,String] [] ("IMPORT '" ++ ((testDirProblem1 ++ "problem11.csv")) ++ "' :: (String,String) WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))
        putStrLn "Test #4"
        quickCheck (assertQueryResult (testDirProblem1++"test2.csv") (testDirProblem1 ++ "problem12.csv") "T" [String,String] [] ("IMPORT '" ++ ((testDirProblem1 ++ "problem13.csv")) ++ "' :: (String,String)  WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))        
        putStrLn "Test #5"
        quickCheck (assertQueryResult (testDirProblem1++"test2.csv") (testDirProblem1 ++ "problem12.csv") "T" [String,String] ["'Name'","'Surname'"] ("IMPORT '" ++ ((testDirProblem1 ++ "problem13.csv")) ++ "' :: (String,String) ('Name1','Surname1') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'Name','Surname','Name1','Surname1')"))
        putStrLn "Test #6"
        quickCheck (assertQueryResult (testDirProblem1++"test2.csv") (testDirProblem1 ++ "problem12.csv") "T" [String,String] [] ("IMPORT '" ++ ((testDirProblem1 ++ "problem13.csv")) ++ "' :: (String,String)  WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))
        putStrLn "Test #7"
        quickCheck (assertQueryResult (testDirProblem1++"test3.csv") (testDirProblem1 ++ "problem14.csv") "T" [String,String] ["'N1'","'N2'"] ("IMPORT '" ++ ((testDirProblem1 ++ "problem15.csv")) ++ "' :: (String,String) ('N11','N21') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))
        putStrLn "Test #8"
        quickCheck (assertQueryResult (testDirProblem1++"test4.csv") (testDirProblem1 ++ "problem1.csv") "T" [String,String] ["'Income'","'Context'"] ("IMPORT '" ++ ((testDirProblem1 ++ "problem16.csv")) ++ "' :: (String,String) ('Income1','Context1') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))
        putStrLn "Test #9"
        quickCheck (assertQueryResult (testDirProblem1++"test4.csv") (testDirProblem1 ++ "problem1.csv") "T" [String,String] ["'Income'","'Context'"] ("IMPORT '" ++ ((testDirProblem1 ++ "problem16.csv")) ++ "' :: (String,String) ('Income1','Context1') WITH HEADER AS R; x := GET R(*) PRODUCT GET T(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))
        putStrLn "Test #10"
        quickCheck (assertQueryResult (testDirProblem1++"test4.csv") (testDirProblem1 ++ "problem1.csv") "T" [String,String] [] ("IMPORT '" ++ ((testDirProblem1 ++ "problem16.csv")) ++ "' :: (String,String)  WITH HEADER AS R; x := GET R(*) PRODUCT GET T(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))
        putStrLn "Test #11"
        quickCheck (assertQueryResult (testDirProblem1++"test4.csv") (testDirProblem1 ++ "problem1.csv") "T" [String,String] ["'Income'","'Context'"] ("IMPORT '" ++ ((testDirProblem1 ++ "problem16.csv")) ++ "' :: (String,String) ('Income1','Context1') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'Income','Context','Income1','Context1')"))
        putStrLn "Test #12"
        quickCheck (assertQueryResult (testDirProblem1++"test4.csv") (testDirProblem1 ++ "problem1.csv") "T" [String,String] ["'Income'","'Context'"] ("IMPORT '" ++ ((testDirProblem1 ++ "problem16.csv")) ++ "' :: (String,String) ('Income1','Context1') WITH HEADER AS R; x := GET R(*) PRODUCT GET T(*) GIVEN True = True; GET x(*) WHERE ORDERBY(ASC,'Income','Context','Income1','Context1')"))
        
        let testDirProblem2 = "C:\\Users\\tamaj\\OneDrive\\TestsSQL-Like\\problem2\\" 
        putStrLn "Test #1"
        quickCheck (assertQueryResult (testDirProblem2++"test1.csv") (testDirProblem2 ++ "problem21.csv") "T" [String,String,String] ["'Name1'","'Name2'","'Surname'"] "GET T('C3','C1') WHERE 'C1' = 'C2', ORDERBY(ASC,'C3','C1')")
        putStrLn "Test #2"
        quickCheck (assertQueryResult (testDirProblem2++"test1.csv") (testDirProblem2 ++ "problem21.csv") "T" [String,String,String] [] "GET T('C3','C1') WHERE 'C1' = 'C2',ORDERBY(ASC,'C3','C1')")
        putStrLn "Test #3"
        quickCheck (assertQueryResult (testDirProblem2++"test1.csv") (testDirProblem2 ++ "problem21.csv") "T" [String,String,String] ["'Name1'","'Name2'","'Surname'"] "GET T('Surname','Name1') WHERE 'Name1' = 'Name2',ORDERBY(ASC,'Surname','Name1')")
        putStrLn "Test #4"
        quickCheck (assertQueryResult (testDirProblem2++"test1.csv") (testDirProblem2 ++ "problem21.csv") "T" [String,String,String] ["'Name1'","'Name2'","'Surname'"] "GET T('Surname','Name1') WHERE 'Name1' = 'Name2',ORDERBY(ASC,'C3','C1')")
        putStrLn "Test #5"
        quickCheck (assertQueryResult (testDirProblem2++"test2.csv") (testDirProblem2 ++ "problem22.csv") "T" [String,String,String] ["'N1'","'N2'","'N3'"] "GET T('C3','C1') WHERE 'C1' = 'C2',ORDERBY(ASC,'C3','C1')")
        putStrLn "Test #6"
        quickCheck (assertQueryResult (testDirProblem2++"test2.csv") (testDirProblem2 ++ "problem22.csv") "T" [String,String,String] ["'N1'","'N2'","'N3'"] "GET T('N3','N1') WHERE 'N1' = 'N2',ORDERBY(ASC,'N3','N1')")
        putStrLn "Test #7"
        quickCheck (assertQueryResult (testDirProblem2++"test2.csv") (testDirProblem2 ++ "problem22.csv") "T" [String,String,String] [] "GET T('C3','C1') WHERE 'C1' = 'C2',ORDERBY(ASC,'C3','C1')")
        putStrLn "Test #8"
        quickCheck (assertQueryResult (testDirProblem2++"test3.csv") (testDirProblem2 ++ "problem23.csv") "T" [String,String,String] [] "GET T('C3','C1') WHERE 'C1' = 'C2',ORDERBY(ASC,'C3','C1')")
        putStrLn "Test #9"
        quickCheck (assertQueryResult (testDirProblem2++"test3.csv") (testDirProblem2 ++ "problem23.csv") "T" [String,String,String] ["'N1'","'N2'","'N3'"] "GET T('N3','N1') WHERE 'N1' = 'N2',ORDERBY(ASC,'N3','N1')")
        putStrLn "Test #10"
        quickCheck (assertQueryResult (testDirProblem2++"test3.csv") (testDirProblem2 ++ "problem23.csv") "T" [String,String,String] ["'N1'","'N2'","'N3'"] "GET T('C3','C1') WHERE 'C1' = 'C2',ORDERBY(ASC,'C3','C1')")
        
        -- tests for problem 3
        let testDirProblem3 = "C:\\Users\\tamaj\\OneDrive\\TestsSQL-Like\\problem3\\" 
        putStrLn "Test #1"
        quickCheck (assertQueryResult (testDirProblem3++"test1.csv") (testDirProblem3 ++ "problem31.csv") "T" [String,String,String,String] ["'N1'","'N2'","'N3'","'N4'"] ("IMPORT '" ++ ((testDirProblem3 ++ "problem32.csv")) ++ "' :: (String,String,String,String) ('N11','N22','N33','N44') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN 'N1' = 'N11'; GET x('C1', IF EMPTY('N2') THEN 'N22' ELSE 'N2' AS 'A', IF EMPTY('N3') THEN 'N33' ELSE 'N3' AS 'B',IF EMPTY('N4') THEN 'N44' ELSE 'N4' AS 'C') WHERE ORDERBY(ASC,'C1','A','B','C')"))
        putStrLn "Test #2"
        quickCheck (assertQueryResult (testDirProblem3++"test1.csv") (testDirProblem3 ++ "problem31.csv") "T" [String,String,String,String] ["'N1'","'N2'","'N3'","'N4'"] ("IMPORT '" ++ ((testDirProblem3 ++ "problem32.csv")) ++ "' :: (String,String,String,String) ('N11','N22','N33','N44') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN 'N1' = 'N11'; y := GET x('C1', IF EMPTY('N2') THEN 'N22' ELSE 'N2', IF EMPTY('N3') THEN 'N33' ELSE 'N3', IF EMPTY('N4') THEN 'N44' ELSE 'N4'); GET y(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))
        putStrLn "Test #3"
        quickCheck (assertQueryResult (testDirProblem3++"test2.csv") (testDirProblem3 ++ "problem33.csv") "T" [String,String,String,String] ["'N1'","'N2'","'N3'","'N4'"] ("IMPORT '" ++ ((testDirProblem3 ++ "problem34.csv")) ++ "' :: (String,String,String,String) ('N11','N22','N33','N44') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN 'N1' = 'N11'; GET x('C1', IF EMPTY('N2') THEN 'N22' ELSE 'N2' AS 'A', IF EMPTY('N3') THEN 'N33' ELSE 'N3' AS 'B',IF EMPTY('N4') THEN 'N44' ELSE 'N4' AS 'C') WHERE ORDERBY(ASC,'C1','A','B','C')"))      
        putStrLn "Test #4"
        quickCheck (assertQueryResult (testDirProblem3++"test2.csv") (testDirProblem3 ++ "problem33.csv") "T" [String,String,String,String] ["'N1'","'N2'","'N3'","'N4'"] ("IMPORT '" ++ ((testDirProblem3 ++ "problem34.csv")) ++ "' :: (String,String,String,String) ('N11','N22','N33','N44') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN 'N1' = 'N11'; y := GET x('C1', IF EMPTY('N2') THEN 'N22' ELSE 'N2', IF EMPTY('N3') THEN 'N33' ELSE 'N3', IF EMPTY('N4') THEN 'N44' ELSE 'N4'); GET y(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))
        putStrLn "Test #5"
        quickCheck (assertQueryResult (testDirProblem3++"test3.csv") (testDirProblem3 ++ "problem35.csv") "T" [String,String,String,String] ["'N1'","'N2'","'N3'","'N4'"] ("IMPORT '" ++ ((testDirProblem3 ++ "problem36.csv")) ++ "' :: (String,String,String,String) ('N11','N22','N33','N44') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN 'N1' = 'N11'; GET x('C1', IF EMPTY('N2') THEN 'N22' ELSE 'N2' AS 'A', IF EMPTY('N3') THEN 'N33' ELSE 'N3' AS 'B',IF EMPTY('N4') THEN 'N44' ELSE 'N4' AS 'C') WHERE ORDERBY(ASC,'C1','A','B','C')"))      
        putStrLn "Test #6"
        quickCheck (assertQueryResult (testDirProblem3++"test3.csv") (testDirProblem3 ++ "problem35.csv") "T" [String,String,String,String] ["'N1'","'N2'","'N3'","'N4'"] ("IMPORT '" ++ ((testDirProblem3 ++ "problem36.csv")) ++ "' :: (String,String,String,String) ('N11','N22','N33','N44') WITH HEADER AS R; x := GET T(*) PRODUCT GET R(*) GIVEN 'N1' = 'N11'; y := GET x('C1', IF EMPTY('N2') THEN 'N22' ELSE 'N2', IF EMPTY('N3') THEN 'N33' ELSE 'N3', IF EMPTY('N4') THEN 'N44' ELSE 'N4'); GET y(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')"))
      
      

      
        -- tests for problem 4
        let testDirProblem4 = "C:\\Users\\tamaj\\OneDrive\\TestsSQL-Like\\problem4\\" 
        putStrLn "Test #1"
        quickCheck (assertQueryResult (testDirProblem4++"test1.csv") (testDirProblem4 ++ "problem41.csv") "T" [String,String] ["'Name1'","'Surname'"] "GET T(*) WHERE NOTEMPTY('C2'), ORDERBY(ASC,'C1','C2')")
        putStrLn "Test #2"
        quickCheck (assertQueryResult (testDirProblem4++"test1.csv") (testDirProblem4 ++ "problem41.csv") "T" [String,String] ["'Name1'","'Surname'"] "GET T('C1','C2') WHERE NOTEMPTY('Surname'), ORDERBY(ASC,'Name1','Surname')")
        putStrLn "Test #3"
        quickCheck (assertQueryResult (testDirProblem4++"test1.csv") (testDirProblem4 ++ "problem41.csv") "T" [String,String] ["'Name1'","'Surname'"] "GET T('Name1','Surname') WHERE NOTEMPTY('Surname'), ORDERBY(ASC,'Name1','C2')")       
        putStrLn "Test #4"
        quickCheck (assertQueryResult (testDirProblem4++"test1.csv") (testDirProblem4 ++ "problem41.csv") "T" [String,String] [] "GET T('C1','C2') WHERE NOTEMPTY('C2'), ORDERBY(ASC,'C1','C2')")
        putStrLn "Test #5"
        quickCheck (assertQueryResult (testDirProblem4++"test2.csv") (testDirProblem4 ++ "problem42.csv") "T" [String,String] ["'Name1'","'Surname'"] "GET T(*) WHERE NOTEMPTY('C2'), ORDERBY(ASC,'C1','C2')")
        putStrLn "Test #6"
        quickCheck (assertQueryResult (testDirProblem4++"test2.csv") (testDirProblem4 ++ "problem42.csv") "T" [String,String] ["'Name1'","'Surname'"] "GET T('C1','C2') WHERE NOTEMPTY('Surname'), ORDERBY(ASC,'Name1','Surname')")
        putStrLn "Test #7"
        quickCheck (assertQueryResult (testDirProblem4++"test2.csv") (testDirProblem4 ++ "problem42.csv") "T" [String,String] ["'Name1'","'Surname'"] "GET T('Name1','Surname') WHERE NOTEMPTY('Surname'), ORDERBY(ASC,'Name1','C2')")
        putStrLn "Test #8"
        quickCheck (assertQueryResult (testDirProblem4++"test2.csv") (testDirProblem4 ++ "problem42.csv") "T" [String,String] [] "GET T('C1','C2') WHERE NOTEMPTY('C2'), ORDERBY(ASC,'C1','C2')")
       
       -- tests for problem 5
        let testDirProblem5 = "C:\\Users\\tamaj\\OneDrive\\TestsSQL-Like\\problem5\\"
        putStrLn "Test #1"
        quickCheck (assertQueryResult (testDirProblem5++"test1.csv") (testDirProblem5 ++ "problem51.csv") "T" [String] ["'Name'"] "GET T('Name',\"0\" AS 'Int','Name') WHERE ORDERBY(ASC,'C1','Int')")
        putStrLn "Test #2"
        quickCheck (assertQueryResult (testDirProblem5++"test1.csv") (testDirProblem5 ++ "problem51.csv") "T" [String] ["'Name'"] "GET T('C1',\"0\" AS 'Int','C1') WHERE ORDERBY(ASC,'C1','Int')")
        putStrLn "Test #3"
        quickCheck (assertQueryResult (testDirProblem5++"test1.csv") (testDirProblem5 ++ "problem51.csv") "T" [String] ["'Name'"] "GET (GET T('C1',\"0\",'C1'))(*) WHERE ORDERBY(ASC,'C1','C2','C3')")
        putStrLn "Test #4"
        quickCheck (assertQueryResult (testDirProblem5++"test1.csv") (testDirProblem5 ++ "problem51.csv") "T" [String] ["'Name'"] "x := GET T('C1',\"0\",'C1'); GET x(*) WHERE ORDERBY(ASC,'C1','C2','C3')")
       



       
       
       
       
       

       

       
        
        

        
        
       

        
        
        

        
        
        
        
        
        
        
        
        
      
        


        
        
 
        



        

        
        
        
        
        
        
        
        

        
       
        
        
        
        -- Same tests as above but with large data set
       {- putStrLn "Test #1"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] [] "GET T(*)")
        putStrLn "Test #2"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] [] "GET T('C1')")
        putStrLn "Test #3"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] [] "GET T('C3')")
        putStrLn "Test #4"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] [] "GET T('C1','C3')")
        putStrLn "Test #5"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] [] "GET T('C3','C1')")
        putStrLn "Test #6"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] [] "GET T('C1','C2','C3','C4','C5','C6','C7')")
        putStrLn "Test #7"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Date','Id','Gender','Age','Income','Context','Cost')")
        putStrLn "Test #8"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Date')")
        putStrLn "Test #9"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender')")
        putStrLn "Test #10"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Date','Gender')")
        putStrLn "Test #11"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender','Date')")
        putStrLn "Test #12"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(1,'Gender','Date')")
        putStrLn "Test #13"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(1,'C3','C1')")
        putStrLn "Test #14"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(\"hello\",'C3','C1')")
        putStrLn "Test #15"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(\"hello\",'Gender','Date')")
        putStrLn "Test #16"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(\"hello\",1,\"benny\")")
        putStrLn "Test #17"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(2*3,\"hello\" CONCAT \"hello\")")
        putStrLn "Test #18"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('Gender' CONCAT \"Male\")")
        putStrLn "Test #19"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T('C3' CONCAT \"Male\")")
        putStrLn "Test #20"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(\"Male\" CONCAT 'C3')")
        putStrLn "Test #21"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET T(\"Male\" CONCAT 'Gender')")
        putStrLn "Test #22"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(*))(*)")
        putStrLn "Test #23"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('C1'))(*)")
        putStrLn "Test #24"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('C1'))('C1')")
        putStrLn "Test #25"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('C1'))('Date')")
        putStrLn "Test #26"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T('C1' AS 'R'))('R')")
        putStrLn "Test #27"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(\"hello\",'C3','C1'))(*)")
        putStrLn "Test #28"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(\"hello\",'C3','C1'))('C1','C2','C3')")
        putStrLn "Test #29"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(\"hello\" AS 'T','C3' AS 'R','C1' AS 'Q'))('T','R','Q')")
        putStrLn "Test #30"
        quickCheck (assertQueryResultWithLength 486104 impressionFilePathLarge "T" [String,String,String,String,String,String,String] ["'Date'","'Id'","'Gender'","'Age'","'Income'","'Context'","'Cost'"] "GET (GET T(\"hello\" AS 'T','C3' AS 'R','C1' AS 'Q'))('T','C2','Q')") -}
        

        
        
        
        
        

        

        

        




        
        




        
        
        


        

        

        

        
        


        


        
        
        


        
        
        
        
        
        
        
        
        
        


