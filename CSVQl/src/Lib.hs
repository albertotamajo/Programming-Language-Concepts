module Lib where
import Grammar
import Control.Applicative
import Control.Exception
import qualified Text.Read as T

import Data.List

data SQLLikeException = TypeCheckEx TypeCheckerException

data TypeCheckerException =  HeaderException String [String] [Type] | NonExistingTableException String | NonSingletonTableInAggregation AggregateFunction Type 
                           | UnionException Type Type | MergeException Type Type | NonQueryVariable String Type | NonExistingQueryVariable String
                           | NonExistingColumnInt Int | NonExistingColumnName String | DuplicatedColumnInGetException String | NonSingletonTableInExpression Type
                           | NonExistingVariable String | IncorrectArgsStringOperatorException StringOperator Type Type | IncorrectArgsBiLogicOpException BinaryLogicOperator Type Type
                           | IncorrectArgsCompOpException ComparisonOperator Type Type | IncorrectArgsArithOpException ArithmeticOperator Type Type
                           | IncorrectArgUnaryLogicOpException UnaryLogicOperator Type | WrongColumnTypeInAggregation AggregateFunction Type | TypeCheckerException Int TypeCheckerException
                           | IncorrectArgsIfThenElseCol Type Type 
                           
                           
instance Show SQLLikeException where
    show (TypeCheckEx tcke) = show tcke
    
instance Exception SQLLikeException 

instance Show TypeCheckerException where
    show (HeaderException tName head types) = "The declared header for table " ++ tName ++ " is " ++ (show head) ++ ".\nThe declared types for table " ++ tName  ++ " are " ++ (show types) ++ ".\nThe length of the declared header for table " ++ tName ++ " is " ++ (show $ length head) ++ " while the length of the declared types is " ++ (show $ length types) ++ ".\nThe length of the header and the types should be equivalent"
    show (NonExistingTableException tName) = "The table " ++ tName ++ " has not been defined prior to its use"
    show (NonSingletonTableInAggregation aggreFun tType) = "The argument of the aggregate function " ++ (show aggreFun) ++ " is not a singleton table as it contains more than one column.\nIndeed, its type is " ++ show tType 
    show (UnionException tType1 tType2) = "The arguments of the UNION function do not have the same type.\nThe first argument has type  " ++ show tType1 ++ "\nThe second argument has type " ++ show tType2
    show (MergeException tType1 tType2) = "The arguments of the MERGE function do not have the same type.\nThe first argument has type  " ++ show tType1 ++ "\nThe second argument has type " ++ show tType2
    show (NonQueryVariable varName t) = "The variable " ++ varName ++ " is supposed to be a query. However, its type is " ++ show t
    show (NonExistingQueryVariable varName) = "The variable " ++ varName ++ " is supposed to be a query. However, it has not been defined prior to its use"
    show (NonExistingColumnInt n) = "The column " ++ show n ++ " does not exist in the queried table"
    show (NonExistingColumnName xs) = "The column " ++ show xs ++ " does not exist in the queried table"
    show (DuplicatedColumnInGetException xs) = "The column " ++ show xs ++ " is instantiated twice inside the GET function. However, it is possible to instantiate a column only once"
    show (NonSingletonTableInExpression tType) = "Tables used inside expressions need to be singleton. However, a table having type  " ++ show tType ++ " is used."
    show (NonExistingVariable xs) = "The variable " ++ xs ++ " has not been defined prior to its use"
    show (WrongColumnTypeInAggregation aggreFun t) = case aggreFun of
                                                       Sum -> "The SUM aggregate function can be performed only with a column having type Int or Float. However, it is used for a column having type " ++ show t
                                                       Avg -> "The AVG aggregate function can be performed only with a column having type Int or Float. However, it is used for a column having type " ++ show t
    
    show (IncorrectArgsStringOperatorException strOp t1 t2) = "The first argument of the string operator " ++ show strOp ++ " has type " ++ show t1 ++ " .\nThe second argument has type " ++ show t2 ++ " .\nHowever, the string operator " ++ show strOp ++ " takes two arguments of type String"
    show (IncorrectArgsBiLogicOpException biLogOp t1 t2) = "The first argument of the binary logic operator " ++ show biLogOp ++ " has type " ++ show t1 ++ " .\nThe second argument has type " ++ show t2 ++ " .\nHowever, the binary logic operator " ++ show biLogOp ++ " takes two arguments of type String"
    show (IncorrectArgsCompOpException compOp t1 t2) = "The first argument of the comparison operator " ++ show compOp ++ " has type " ++ show t1 ++ " .\nThe second argument has type " ++ show t2 ++ " .\nHowever, the comparison operator " ++ show compOp ++ " takes two arguments of the same type"
    show (IncorrectArgsArithOpException arithOp t1 t2) = "The first argument of the arithmetic operator " ++ show arithOp ++ " has type " ++ show t1 ++ " .\nThe second argument has type " ++ show t2 ++ " .\nHowever, the arithmetic operator " ++ show arithOp ++ " takes two arguments of type Int or Float"
    show (IncorrectArgUnaryLogicOpException unLogicOp t1) = "The argument of the unary logic operator " ++ show unLogicOp ++ " has type " ++ show t1 ++ " .\nHowever, the unary logic operator " ++ show unLogicOp ++ " takes an argument of type Bool"
    show (IncorrectArgsIfThenElseCol type1 type2) = "The first expression in the If-Then-Else statement has type " ++ show type1 ++ " .\nThe second expression has type " ++ show type2 ++ " .\nHowever, the expressions in the If-Then-Else statement should have the same type"
    
instance Exception TypeCheckerException

data RunTimeException = NotSingletonTableMultipleRowsException | NotSingletonTableNoRowException | ColumnTypeException String (Int,Int,Type,String)

instance Show RunTimeException where
    show (NotSingletonTableMultipleRowsException) = "An expected singleton table actually contains more than one row in its unique column"
    show (NotSingletonTableNoRowException) = "An expected singleton table actually contains no row"
    show (ColumnTypeException table (row,col,t,entry)) = "Wrong entry type in table " ++ table ++ ".\nExpected type of entry at " ++ show row ++ ":" ++ show col ++ " is " ++ show t ++ " but its value is: " ++ entry
    
instance Exception RunTimeException


data Result = ResInt {getInt :: Int} | ResFloat {getFloat :: Float} | ResString {getString :: String} | ResBool {getBool :: Bool} | ResQuery {getTabList ::TableList}
instance Eq Result where
    ResInt n1 == ResInt n2 = n1 == n2
    ResFloat f1 == ResFloat f2 = f1 == f2
    ResString xs == ResString ys = xs == ys
    ResBool b1 == ResBool b2 = b1 == b2
    ResQuery q1 == ResQuery q2 = q1 == q2
    
instance Ord Result where
    ResInt n1 `compare` ResInt n2 = n1 `compare` n2
    ResFloat f1 `compare` ResFloat f2 = f1 `compare` f2
    ResString xs `compare` ResString ys = xs `compare` ys
    ResBool b1 `compare` ResBool b2 = b1 `compare` b2
    ResString "" `compare` ResInt n1 = (show n1) `compare` ""
    ResString "" `compare` ResFloat f2 = (show f2) `compare` ""
    ResString "" `compare` ResBool b1 = (show b1) `compare` ""
    ResInt n1 `compare` ResString "" = "" `compare` (show n1)
    ResFloat f1 `compare` ResString "" = "" `compare` (show f1)
    ResBool b1 `compare` ResString "" = "" `compare` (show b1)
    
type TableEnvironment = [(TableName, TableList)]
type VariableEnvironment = [(VarName,Expression)]
type TableList = ([String],[Type],[[String]])

type TypeEnvironment = [(VarName,Type)]
type ColumnTypeEnvironment = [(ColumnName,Type)]
 
--Function that evaluates a program
evalProgram :: Program -> IO [TableList]
evalProgram [] = return [] -- if the program is empty then just return an empty list
evalProgram (x:xs) = evalProgram' [] [] [] (x:xs)
                   where
                        evalProgram' :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> Program -> IO [TableList]
                        evalProgram' _ _ _ [] = return [] -- if the program is empty then just return an empty list
                        evalProgram' tEnvs vEnvs typeEnv ((Import tSource types header WithHeader tName) : ps) = do
                                                                                                                  table <- readCsvFile tSource
                                                                                                                  case checkColsEntriesType (tail table) types of
                                                                                                                    Left tuple -> throw (ColumnTypeException tName tuple)
                                                                                                                    Right _ ->do
                                                                                                                                remainingProgram <-  evalProgram' ((tName,(header, types, tail table)):tEnvs) vEnvs ((tName,(Table header types)): typeEnv) ps  -- here the first line of the table is skipped because it is the header
                                                                                                                                return $ ([],[],[]) : remainingProgram
                                                                                                    
                        evalProgram' tEnvs vEnvs typeEnv ((Import tSource types header WithoutHeader tName) : ps) = do
                                                                                                                     table <- readCsvFile tSource  
                                                                                                                     case checkColsEntriesType table types of
                                                                                                                       Left tuple -> throw (ColumnTypeException tName tuple)
                                                                                                                       Right _ -> do
                                                                                                                                    remainingProgram <-  evalProgram' ((tName,(header,types, table)):tEnvs) vEnvs ((tName,(Table header types)): typeEnv) ps  -- here the first line of the table is not skipped because it is not the header
                                                                                                                                    return $ ([],[],[]) : remainingProgram      
                                                                                                    
                        evalProgram' tEnvs vEnvs typeEnv ((VarAssign varName expression) : ps) = do
                                                                                                  remainingProgram <- evalProgram' tEnvs ((varName,expression) : vEnvs) ((varName,typeOfExpression typeEnv expression): typeEnv) ps 
                                                                                                  return $ ([],[],[]) : remainingProgram
                                                                                           
                        evalProgram' tEnvs vEnvs typeEnv (S query : ps) = do
                                                                           let tableList = evalQuery tEnvs vEnvs typeEnv query
                                                                           remainingProgram <- evalProgram' tEnvs vEnvs typeEnv ps
                                                                           return $ tableList : remainingProgram
                                                                   
                                                                    
--Function that evaluates a Query
evalQuery :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> Query -> TableList

evalQuery tEnvs vEnvs typeEnv (Union q1 q2) | types1 == types2 =  (headQ1, types1, nub $ tableQ1 ++ tableQ2)
                                            | otherwise = error "Cannot perform UNION with tables having different types"
                                            where
                                              (headQ1, types1, tableQ1) = evalQuery tEnvs vEnvs typeEnv q1
                                              (_, types2,tableQ2) = evalQuery tEnvs vEnvs typeEnv q2
                                      
evalQuery tEnvs vEnvs typeEnv (Merge q1 q2) | types1 == types2 = (headQ1, types1,tableQ1 ++ tableQ2)
                                            | otherwise = error "Cannot perform MERGE with tables having different types"
                                            where
                                             (headQ1, types1, tableQ1) = evalQuery tEnvs vEnvs typeEnv q1
                                             (_, types2, tableQ2) = evalQuery tEnvs vEnvs typeEnv q2
                                      
evalQuery tEnvs vEnvs typeEnv (Get Distinct table cols conds) = case table of
                                                                     TName tName ->  case lookup tName tEnvs of
                                                                                       Just (head, types,rows) -> (newHead, newTypes, (computeFirstLastLimit (nub (transformTableListGet tEnvs vEnvs typeEnv head types rows cols conds)) conds))
                                                                                       Nothing -> error "No table variable found" 
                                                                                 
                                                                     TQuery query -> let (head, types, rows) = evalQuery tEnvs vEnvs typeEnv query in  (newHead, newTypes, (computeFirstLastLimit (nub (transformTableListGet tEnvs vEnvs typeEnv head types rows cols conds)) conds))
                                                                where
                                                                  (Table newHead newTypes) = typeOfQuery typeEnv (Get NoDistinct table cols conds)

                                                        
evalQuery tEnvs vEnvs typeEnv (Get NoDistinct table cols conds ) = case table of
                                                                     TName tName ->  case lookup tName tEnvs of
                                                                                       Just (head, types,rows) -> (newHead, newTypes, (computeFirstLastLimit (transformTableListGet tEnvs vEnvs typeEnv head types rows cols conds) conds))
                                                                                       Nothing -> error "No table variable found" 
                                                                                 
                                                                     TQuery query -> let (head, types, rows) = evalQuery tEnvs vEnvs typeEnv query in  (newHead, newTypes, ( computeFirstLastLimit (transformTableListGet tEnvs vEnvs typeEnv head types rows cols conds) conds))
                                                                where
                                                                  (Table newHead newTypes) = typeOfQuery typeEnv (Get NoDistinct table cols conds)
                                                                         
                                                                                                                                                       
evalQuery tEnvs vEnvs typeEnv (ProdQuery prod q1 q2 cond) = case prod of
                                                          Product -> productJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 cond
                                                          InnerProduct -> innerProductJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 cond
                                                          LeftProduct -> leftProductJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 cond
                                                          RightProduct -> rightProductJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 cond
                                                          FullProduct -> fullProductJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 cond
                                                         where
                                                           (head1,types1,rows1) = evalQuery tEnvs vEnvs typeEnv q1
                                                           (head2,types2,rows2) = evalQuery tEnvs vEnvs typeEnv q2
                                                       
evalQuery tEnvs vEnvs typeEnv (VarQuery var) = case lookup var vEnvs of
                                                 Just (EQuery query) -> evalQuery tEnvs vEnvs typeEnv query
                                                 Just (EVar var2) -> evalQuery tEnvs vEnvs typeEnv (VarQuery var2)
                                                 _ -> error "The variable is not a query"
                                        
evalQuery tEnvs vEnvs typeEnv (AggreQuery aggreFun query) = case aggreFun of
                                                              Sum -> returnTable $ sumAggregation typ fstCol
                                                              Count -> returnTable $ countAggregation fstCol
                                                              Avg -> returnTable $ avgAggregation typ fstCol
                                                              Min -> returnTable $ minAggregation typ fstCol
                                                              Max -> returnTable $ maxAggregation typ fstCol
                                                         where
                                                          (header, types, tabList ) = evalQuery tEnvs vEnvs typeEnv query
                                                          typ = head types
                                                          fstCol = map head tabList
                                                          returnTable xs = (header,types,[[xs]])
                                                                                                                                                                                                                                                                                                                               

--Function that applies the first or last limits if applicable
computeFirstLastLimit :: [[String]] -> GetConditions -> [[String]]
computeFirstLastLimit rows (_,Just groupOrdLimits) = case (firstLimit,lastLimit) of
                                                       ([],[]) -> rows
                                                       ([n],_) -> take n rows
                                                       (_,[n]) -> take n $ reverse rows
                                                   where
                                                     firstLimit = [n | First n <- groupOrdLimits]
                                                     lastLimit = [n | Last n <- groupOrdLimits]
                                                     
computeFirstLastLimit rows _ = rows
                                                             
--Function that transforms a table. Effectively it computes the get operation
transformTableListGet :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [String] -> [Type] -> [[String]] -> Columns -> GetConditions -> [[String]]
transformTableListGet tEnvs vEnvs typeEnv head types rows cols (Nothing,Nothing) = map snd (evalColumnsRows tEnvs vEnvs typeEnv head types rows cols)  
transformTableListGet tEnvs vEnvs typeEnv head types rows cols (Just rowF, Nothing) = transformTableListGetRowFilter tEnvs vEnvs typeEnv colEnvsRows head types rows cols rowF
                                                                            where
                                                                              colEnvsRows = evalColumnsRows tEnvs vEnvs typeEnv head types rows cols
                                                                              
transformTableListGet tEnvs vEnvs typeEnv head types rows cols (Nothing, Just groupOrdLimits) = result
                                                                                      where
                                                                                        result = groupOrder tEnvs vEnvs typeEnv head types rows cols groupOrdLimits
                                                                                        colEnvsRows = evalColumnsRows tEnvs vEnvs typeEnv head types rows cols
                                                                                        colEnvs = map fst colEnvsRows
                                                                                        groupOrder :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [String] -> [Type] -> [[String]] -> Columns -> [GroupOrderLimit] -> [[String]]
                                                                                        groupOrder tEnvs vEnvs typeEnv head types rows cols groupLimits | null groupByCols  =  orderBy tEnvs vEnvs typeEnv colEnvs head types rows rowsToOutput groupLimits
                                                                                                                                                        | otherwise = orderBy tEnvs vEnvs typeEnv newColEnvs  head types  firstRows newRows groupLimits
                                                                                                                                                        where
                                                                                                                                                          groupByCols = [fs | GroupBy fs <- groupLimits]
                                                                                                                                                          --groupByRows :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [[(String,Result)]] -> [String] -> [Type] -> [[String]] -> [Column] -> [[[String]]]

                                                                                                                                                          grouppedRows = groupByRows tEnvs vEnvs typeEnv colEnvs head types rows (Prelude.head groupByCols)
                                                                                                                                                          firstRows = map Prelude.head grouppedRows
                                                                                                                                                          evaluatedEnvsRows = map (\f -> f cols)(map (evalColumnsRows tEnvs vEnvs typeEnv head types) grouppedRows)
                                                                                                                                                          newColEnvsRows = map Prelude.head evaluatedEnvsRows
                                                                                                                                                          newColEnvs = map fst newColEnvsRows
                                                                                                                                                          newRows = map snd newColEnvsRows 
                                                                                                                                                  
                                                                                                                                                          rowsToOutput = transformTableListGet tEnvs vEnvs typeEnv head types rows cols (Nothing,Nothing)
                                                                                                                                                  
                                                                                        orderBy :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [[(String,Result)]] -> [String] -> [Type] -> [[String]] -> [[String]] -> [GroupOrderLimit] -> [[String]]
                                                                                        orderBy tEnvs vEnvs typeEnv colEnvs head types rows newRows groupLimits | null orderByCols = newRows
                                                                                                                                                                | otherwise = orderByRows tEnvs vEnvs typeEnv colEnvs head types rows newRows orderCols order
                                                                                                                                                                where
                                                                                                                                                                  orderByCols = [(ord,ps) | OrderBy ord ps <- groupLimits] :: [(Order, [Column])]
                                                                                                                                                                  (order,orderCols) = Prelude.head orderByCols

transformTableListGet tEnvs vEnvs typeEnv head types rows cols (Just rowF, Just groupOrdLimits) = transformTableListGet tEnvs vEnvs typeEnv head types rowsFiltered cols (Nothing, Just groupOrdLimits)
                                                                                        where   
                                                                                          colEnvsRows = evalColumnsRows tEnvs vEnvs typeEnv head types rows cols
                                                                                          rowsFiltered = transformTableListGetRowFilterOldRows tEnvs vEnvs typeEnv colEnvsRows head types rows cols rowF                                                                                    
                                                                                                                                                          
                                                                                                                                                          
                                                                                                                                                  
                                                                                                                                                  
                                                                                                                                                  
                                                                                                                                                  
--This function is the same as the one below but it outputs the filtered hold rows rather than the new ones                                                                                                                                                  
transformTableListGetRowFilterOldRows :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [([(String,Result)],[String])] -> [String] -> [Type] -> [[String]] -> Columns -> RowFilters -> [[String]]
transformTableListGetRowFilterOldRows tEnvs vEnvs typeEnv colEnvsRows head types rows cols rowF = transformTableListGetRowFilterOldRows' tEnvs vEnvs typeEnv colEnvsRows head types rows rows cols rowF
                                                                                                                     where
                                                                                                                        transformTableListGetRowFilterOldRows' :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [([(String,Result)],[String])] -> [String] -> [Type] -> [[String]] -> [[String]] -> Columns -> RowFilters -> [[String]]
                                                                                                                        transformTableListGetRowFilterOldRows' _ _ _ [] _ _ _ _ _ _ = []
                                                                                                                        transformTableListGetRowFilterOldRows' tEnvs vEnvs typeEnv ((colEnv,row) : colEnvsRows) head types (r:rows) allRows cols rowF | computeRowFilters tEnvs vEnvs typeEnv colEnv head types r allRows rowF = r : transformTableListGetRowFilterOldRows' tEnvs vEnvs typeEnv colEnvsRows head types rows allRows cols rowF
                                                                                                                                                                                                                                                      | otherwise = transformTableListGetRowFilterOldRows' tEnvs vEnvs typeEnv colEnvsRows head types rows allRows cols rowF
                                                                                                             
transformTableListGetRowFilter :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [([(String,Result)],[String])] -> [String] -> [Type] -> [[String]] -> Columns -> RowFilters -> [[String]]
transformTableListGetRowFilter _ _ _ [] _ _ _ _ _ = [] 
transformTableListGetRowFilter tEnvs vEnvs typeEnv colEnvsRows head types rows cols rowF = transformTableListGetRowFilter' tEnvs vEnvs typeEnv colEnvsRows head types rows rows cols rowF
                                                                                                                     where
                                                                                                                        transformTableListGetRowFilter' :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [([(String,Result)],[String])] -> [String] -> [Type] -> [[String]] -> [[String]] -> Columns -> RowFilters -> [[String]]
                                                                                                                        transformTableListGetRowFilter' _ _ _ [] _ _ _ _ _ _ = []
                                                                                                                        transformTableListGetRowFilter' tEnvs vEnvs typeEnv ((colEnv,row) : colEnvsRows) head types (r:rows) allRows cols rowF | computeRowFilters tEnvs vEnvs typeEnv colEnv head types r allRows rowF = row : transformTableListGetRowFilter' tEnvs vEnvs typeEnv colEnvsRows head types rows allRows cols rowF
                                                                                                                                                                                                                                               | otherwise = transformTableListGetRowFilter' tEnvs vEnvs typeEnv colEnvsRows head types rows allRows cols rowF                                                                                                                                                                  

--Function that performs the product between two tableList
productJoin :: TableEnvironment -> VariableEnvironment -> TypeEnvironment ->  [String] -> [String] -> [Type] -> [Type] -> [[String]] -> [[String]] -> RowFilters -> TableList  
productJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 rowF = ((head1++head2),(types1++types2),filteredJoinRows) 
                                                                           where
                                                                             joinRows = cartesianProduct rows1 rows2
                                                                             filteredJoinRows = filterRows tEnvs vEnvs typeEnv (head1++head2) (types1++types2) joinRows joinRows rowF
                                                                             filterRows :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [String] -> [Type] -> [[String]] -> [[String]] -> RowFilters -> [[String]]
                                                                             filterRows _ _ _ _ _ _ [] _ = []
                                                                             filterRows tEnvs vEnvs typeEnv head types allRows (r:rows) rowF | computeRowFilters tEnvs vEnvs typeEnv [] head types r allRows rowF = r : filterRows tEnvs vEnvs typeEnv head types allRows rows rowF 
                                                                                                                                             | otherwise = filterRows tEnvs vEnvs typeEnv head types allRows rows rowF

--Function that performs the inner product between two tableList
innerProductJoin :: TableEnvironment -> VariableEnvironment -> TypeEnvironment ->  [String] -> [String] -> [Type] -> [Type] -> [[String]] -> [[String]] -> RowFilters -> TableList 
innerProductJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 rowF = productJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 rowF

-- Function that performs the left product between two tableList
leftProductJoin :: TableEnvironment -> VariableEnvironment -> TypeEnvironment ->  [String] -> [String] -> [Type] -> [Type] -> [[String]] -> [[String]] -> RowFilters -> TableList 
leftProductJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 rowF = (head,types,(rows ++ [r++nulls | r <- rowsToAdd]))
                                                                               where
                                                                                 len1 = length types1
                                                                                 len2= length types2
                                                                                 nulls = take len2 (repeat "")
                                                                                 rowsToAdd = rows1 \\ (map (take len1) rows)
                                                                                 (head,types,rows) = productJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 rowF

-- Function that performs the right product between two tableList
rightProductJoin :: TableEnvironment -> VariableEnvironment -> TypeEnvironment ->  [String] -> [String] -> [Type] -> [Type] -> [[String]] -> [[String]] -> RowFilters -> TableList 
rightProductJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 rowF = (head,types,(rows ++ [nulls ++ r | r <- rowsToAdd]))
                                                                               where
                                                                                 len1 = length types1
                                                                                 len2= length types2
                                                                                 nulls = take len1 (repeat "")
                                                                                 rowsToAdd = rows2 \\ (map (drop len1) rows)
                                                                                 (head,types,rows) = productJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 rowF

-- Function that performs the full product between two tableList
fullProductJoin :: TableEnvironment -> VariableEnvironment -> TypeEnvironment ->  [String] -> [String] -> [Type] -> [Type] -> [[String]] -> [[String]] -> RowFilters -> TableList 
fullProductJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 rowF = (head,types,(rows ++ [nulls ++ r | r <- rowsToAdd]))
                                                                               where
                                                                                 len1 = length types1
                                                                                 len2= length types2
                                                                                 nulls = take len1 (repeat "")
                                                                                 rowsToAdd = rows2 \\ (map (drop len1) rows) 
                                                                               
                                                                                 (head,types,rows) = leftProductJoin tEnvs vEnvs typeEnv head1 head2 types1 types2 rows1 rows2 rowF
                                                                                 

--Function that computes the cartesian product between two list of lists
cartesianProduct :: [[a]] -> [[a]] -> [[a]]
cartesianProduct xs ys = (++) <$> xs <*> ys                   
                                                                                                                        
--Function that computes the sum aggregation
sumAggregation :: Type -> [String] -> String
sumAggregation _ [] = ""
sumAggregation Int xs = let ints = (map read (filter (\x -> x /= "") xs) :: [Int]) in if ints /= [] then show (sum ints) else ""
sumAggregation Float xs = let fts = (map read (filter (\x -> x /= "") xs) :: [Float]) in if fts /= [] then show (sum fts) else ""
                        
--Function that computes the avg aggregation
avgAggregation :: Type -> [String] -> String
avgAggregation _ [] = ""
avgAggregation Int xs = case sum of
                           "" -> ""
                           _ -> let sumRead = read sum :: Float in show (sumRead / fromIntegral (length (filter (\x -> x /= "") xs)))
                      where
                        sum = sumAggregation Int xs 
                        
avgAggregation Float xs = case sum of
                            "" -> ""
                            _ -> let sumRead = read sum :: Float  in show (sumRead / fromIntegral (length (filter (\x -> x /= "") xs))) 
                        where
                          sum = (sumAggregation Float xs)
                        
                        
--Function that computes the count aggregation                      
countAggregation :: [String] -> String
countAggregation xs = show $ length xs


--Function that computes the min aggregation
minAggregation :: Type -> [String] -> String
minAggregation _ [] = ""
minAggregation Int xs = let ints = (map read (filter (\x -> x /= "") xs) :: [Int]) in if ints /= [] then show $ minimum ints else ""
minAggregation Float xs = let fts = (map read (filter (\x -> x /= "") xs) :: [Float]) in if fts /= [] then show $ minimum fts else ""
minAggregation String xs =  let filtxs = filter (\x -> x /= "") xs in if filtxs /= [] then minimum filtxs else ""
minAggregation Bool xs = let bools = (map read (filter (\x -> x /= "") xs) :: [Bool]) in if bools /= [] then show $ minimum bools else ""


--Function that computes the max aggregation
maxAggregation :: Type -> [String] -> String
maxAggregation _ [] = ""
maxAggregation Int xs = let ints = (map read (filter (\x -> x /= "") xs) :: [Int]) in if ints /= [] then show $ maximum ints else ""
maxAggregation Float xs = let fts = (map read (filter (\x -> x /= "") xs) :: [Float]) in if fts /= [] then show $ maximum fts else ""
maxAggregation String xs = maximum xs
maxAggregation Bool xs = let bools = (map read (filter (\x -> x /= "") xs) :: [Bool]) in if bools /= [] then show $ maximum bools else ""
                                                                                                                                                                     
                                                                                 
--Function that reads a CSV file
readCsvFile :: String -> IO [[String]]
readCsvFile xs = do
                   source <- readFile xs
                   let rows = lines source
                   let table = map (splitOn ',') rows
                   let tableNoLeadTrailWhiteSpaces = map (map removeLeadTrailWhiteSpaces) table
                   return tableNoLeadTrailWhiteSpaces
                   
                   
--Function that checks whether all entries of the columns of a table have the correct type
checkColsEntriesType :: [[String]] -> [Type] -> Either (Int,Int,Type,String) Bool
checkColsEntriesType xs t = checkColsEntriesType' 0 xs t
                          where
                            checkColsEntriesType' :: Int -> [[String]] -> [Type] -> Either (Int,Int,Type,String) Bool
                            checkColsEntriesType' c xs [] = Right True
                            checkColsEntriesType' c xs (String : ts) = checkColsEntriesType' (c+1) xs ts
                            checkColsEntriesType' c xs (t:ts) = case checkColEntriesType col t of
                                                                  Right _ -> checkColsEntriesType' (c+1) xs ts
                                                                  Left r -> Left (r+1, c+1, t, col !! r)
                                                               where
                                                                 col = (map (!!c) xs)
                                                                  
                            
--Function that checks whether the entries of a column have the correct type
checkColEntriesType :: [String] -> Type -> Either Int Bool
checkColEntriesType xs t = checkColEntriesType' 0 xs t
                       where
                        checkColEntriesType' :: Int -> [String] -> Type -> Either Int Bool
                        checkColEntriesType' _ [] _= Right True
                        
                        checkColEntriesType' n (x:xs) Int | x == "" = checkColEntriesType' (n+1) xs Int
                                                          | otherwise = case T.readMaybe x :: Maybe Int of
                                                                          Just _ -> checkColEntriesType' (n+1) xs Int
                                                                          Nothing -> Left n
                                                            
                        checkColEntriesType' n (x:xs) Float | x == "" = checkColEntriesType' (n+1) xs Float
                                                            | otherwise = case T.readMaybe x :: Maybe Float of
                                                                            Just _ -> checkColEntriesType' (n+1) xs Float
                                                                            Nothing -> Left n  
                                                            
                        checkColEntriesType' n (x:xs) Bool | x == "" = checkColEntriesType' (n+1) xs Bool
                                                           | otherwise = case T.readMaybe x :: Maybe Bool of
                                                                           Just _ -> checkColEntriesType' (n+1) xs Bool
                                                                           Nothing -> Left n                                                            

                                                 
--Function that removes leading and trailing whitespaces
removeLeadTrailWhiteSpaces :: String -> String
removeLeadTrailWhiteSpaces xs = removeTrailingWhiteSpaces $ removeLeadingWhiteSpaces xs

--Function that removes leading whitespaces
removeLeadingWhiteSpaces :: String -> String
removeLeadingWhiteSpaces xs = dropWhile (\x -> x == ' ') xs

--Function that removes trailing white spaces
removeTrailingWhiteSpaces :: String -> String
removeTrailingWhiteSpaces xs = reverse $ removeLeadingWhiteSpaces (reverse xs)
                   
--Function that splits a list of strings on a character                   
splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c ls = (takeWhile (/=c) ls) : splitOn' c (dropWhile (/=c) ls)
 where splitOn' c [] = []
       splitOn' c (x:[]) | x==c = [[]]
       splitOn' c (x:xs) | x==c = splitOn c xs
                         | otherwise = []                   
                   
                  
--Function that evaluates an expression
evalExpression :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> Expression -> Result
evalExpression tEnvs vEnvs typeEnv (EQuery query) = ResQuery (evalQuery tEnvs vEnvs typeEnv query)
evalExpression tEnvs vEnvs typeEnv (EVar var) = case lookup var vEnvs of
                                                  Just e -> evalExpression tEnvs vEnvs typeEnv e
                                                  Nothing -> error ("Variable " ++ var ++ " not found")
                                          
evalExpression tEnvs vEnvs typeEnv (EBool e) = evalBoolExpression tEnvs vEnvs typeEnv e
evalExpression tEnvs vEnvs typeEnv (EString e) = evalStringExpression tEnvs vEnvs typeEnv e 
evalExpression tEnvs vEnvs typeEnv (EArith e) = evalArithExpression tEnvs vEnvs typeEnv e

--Function that evaluates a String expression
evalStringExpression :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> StringExpression -> Result
evalStringExpression _ _ _ (StringLit xs) = ResString xs 
evalStringExpression tEnvs vEnvs typeEnv (StringOp op e1 e2) = computeStrExpression evalE1 evalE2 op
                                                     where
                                                           evalE1 = evalExpression tEnvs vEnvs typeEnv e1
                                                           evalE2 = evalExpression tEnvs vEnvs  typeEnv e2
                                                           
                                                           
strMessNull = "Cannot perform string operation with the empty value"

--Function that computes string expression
computeStrExpression :: Result -> Result -> StringOperator -> Result
computeStrExpression (ResString "") _ _ = ResString ""
computeStrExpression _ (ResString "") _ = ResString ""
computeStrExpression (ResString xs) (ResString ys) op = case op of
                                                          Concat -> ResString (xs++ys)
                                                         
computeStrExpression (ResString xs) (ResQuery tableList) op = case extractUnique tableList of
                                                                "" -> ResString ""
                                                                ys -> computeStrExpression (ResString xs) (ResString ys) op
                                                                
computeStrExpression (ResQuery tableList) (ResString ys) op = case extractUnique tableList of
                                                                "" -> ResString ""
                                                                xs -> computeStrExpression (ResString xs) (ResString ys) op
                                                                                                                                
computeStrExpression (ResQuery tableList1) (ResQuery tableList2) op = case (extractUnique tableList1, extractUnique tableList2) of
                                                                        ("", _) -> ResString ""
                                                                        (_,"") -> ResString ""
                                                                        (xs,ys) -> computeStrExpression (ResString xs) (ResString ys) op
                                                                        
                                                                        
--Function that evaluates a Boolean expression
evalBoolExpression :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> BoolExpression -> Result
evalBoolExpression _ _ _ (BoolTrue) = ResBool True
evalBoolExpression _ _ _ (BoolFalse) = ResBool False
evalBoolExpression tEnvs vEnvs typeEnv (BoolNot e) = case evalE of
                                                       ResBool b -> ResBool (not b)
                                                       ResString "" -> ResString ""
                                                   where
                                                     evalE = evalExpression tEnvs vEnvs typeEnv e 
                                             
evalBoolExpression tEnvs vEnvs typeEnv (BoolOpLogic op e1 e2) = computeLogicBoolExpression evalE1 evalE2 op 
                                                              where
                                                                evalE1 = evalExpression tEnvs vEnvs typeEnv e1
                                                                evalE2 = evalExpression tEnvs vEnvs typeEnv e2
                                                        
evalBoolExpression tEnvs vEnvs typeEnv (BoolOpComp op e1 e2) = computeCompBoolExpression evalE1 evalE2 op
                                                             where
                                                               evalE1 = evalExpression tEnvs vEnvs typeEnv e1
                                                               evalE2 = evalExpression tEnvs vEnvs typeEnv e2
                                                        

--Function that computes a boolean logic expression
computeLogicBoolExpression :: Result -> Result -> BinaryLogicOperator -> Result
computeLogicBoolExpression (ResBool b1) (ResBool b2) op = case op of
                                                            And -> ResBool (b1 && b2)
                                                            Or -> ResBool (b1 || b2)
                                                       
computeLogicBoolExpression (ResBool b1) (ResQuery tableList) op = case extractUnique tableList of
                                                                    "" -> ResString ""
                                                                    xs -> computeLogicBoolExpression (ResBool b1) (ResBool (read xs)) op
                                                               
computeLogicBoolExpression (ResQuery tableList) (ResBool b2) op = case extractUnique tableList of
                                                                    "" -> ResString ""
                                                                    xs -> computeLogicBoolExpression (ResBool (read xs)) (ResBool b2) op
                                                               
computeLogicBoolExpression (ResQuery tableList1) (ResQuery tableList2) op = case (extractUnique tableList1, extractUnique tableList2) of
                                                                            ("", _) -> ResString ""
                                                                            (_,"") -> ResString ""
                                                                            (xs,ys) -> computeLogicBoolExpression (ResBool (read xs)) (ResBool (read ys)) op
                                                                            
computeLogicBoolExpression (ResString "") _ _ = ResString ""
computeLogicBoolExpression _ (ResString "") _ = ResString ""
                                                       
                                    
--Function that evaluates a boolean comparison expression
computeCompBoolExpression :: Result -> Result -> ComparisonOperator -> Result
computeCompBoolExpression (ResInt n1) (ResInt n2) op = computeComparision op n1 n2
computeCompBoolExpression (ResInt n1) (ResQuery tableList) op = case extractUnique tableList of
                                                                  "" -> ResString ""
                                                                  xs -> computeCompBoolExpression (ResInt n1) (ResInt (read xs)) op
                                                                  
computeCompBoolExpression (ResQuery tableList) (ResInt n2) op = case extractUnique tableList of
                                                                  "" -> ResString ""
                                                                  xs -> computeCompBoolExpression (ResInt (read xs)) (ResInt n2) op
                                                                  
computeCompBoolExpression (ResFloat f1) (ResFloat f2) op = computeComparision op f1 f2      
computeCompBoolExpression (ResFloat f1) (ResQuery tableList) op = case extractUnique tableList of 
                                                                    "" -> ResString ""
                                                                    xs -> computeCompBoolExpression (ResFloat f1) (ResFloat (read xs)) op
                                                                    
computeCompBoolExpression (ResQuery tableList) (ResFloat f2) op = case extractUnique tableList of
                                                                    "" -> ResString ""
                                                                    xs -> computeCompBoolExpression (ResFloat (read xs)) (ResFloat f2) op
                                                                    
computeCompBoolExpression (ResString xs) (ResString ys) op = computeComparision op xs ys  
computeCompBoolExpression (ResString xs) (ResQuery tableList) op = case extractUnique tableList of
                                                                     "" -> ResString ""
                                                                     ys -> computeCompBoolExpression (ResString xs) (ResString ys) op
                                                                     
computeCompBoolExpression (ResQuery tableList) (ResString ys) op = case extractUnique tableList of
                                                                     "" -> ResString ""
                                                                     xs -> computeCompBoolExpression (ResString xs) (ResString ys) op
                                                                     
computeCompBoolExpression (ResBool b1) (ResBool b2) op = computeComparision op b1 b2
computeCompBoolExpression (ResBool b1) (ResQuery tableList) op = case extractUnique tableList of
                                                                   "" -> ResString ""
                                                                   xs -> computeCompBoolExpression (ResBool b1) (ResBool (read xs)) op
                                                                   
computeCompBoolExpression (ResQuery tableList) (ResBool b2) op = case extractUnique tableList of
                                                                   "" -> ResString ""
                                                                   xs -> computeCompBoolExpression (ResBool (read xs)) (ResBool b2) op
                                                           
computeCompBoolExpression (ResQuery tableList1) (ResQuery tableList2) op = case (extractUnique tableList1, extractUnique tableList2) of
                                                                            ("",_) -> ResString ""
                                                                            (_,"") -> ResString ""
                                                                            (xs, ys) -> case (t1,t2) of
                                                                                          (Int,Int) -> computeCompBoolExpression (ResInt (read xs)) (ResInt (read ys)) op
                                                                                          (Float,Float) -> computeCompBoolExpression (ResFloat (read xs)) (ResFloat (read ys)) op
                                                                                          (String,String) -> computeCompBoolExpression (ResString xs) (ResString ys ) op
                                                                                          (Bool, Bool) -> computeCompBoolExpression (ResBool (read xs)) (ResBool (read ys)) op
                                                                        where
                                                                            (_,t1:tps1,_) = tableList1
                                                                            (_,t2:tps2,_) = tableList2
                                                                            
computeCompBoolExpression (ResString "") _ _ = ResString ""
computeCompBoolExpression _ (ResString "") _ = ResString ""
                                                                            
                             
--Function that given two values compare them according to an operator and output a result
computeComparision :: Ord a => ComparisonOperator -> a -> a -> Result
computeComparision Eq x y = ResBool $ x==y
computeComparision Diff x y = ResBool $  x/=y
computeComparision GreaterThan x y = ResBool $ x > y
computeComparision LessThan x y = ResBool $ x < y
computeComparision GreaterThanEq x y = ResBool $ x >= y
computeComparision LessThanEq x y = ResBool $ x <= y


--Function that evaluates an arithmetic expression
evalArithExpression :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> ArithExpression -> Result
evalArithExpression _ _ _ (IntLit n) = ResInt n
evalArithExpression _ _ _ (FloatLit f) = ResFloat f
evalArithExpression tEnvs vEnvs typeEnv (ArithOp op e1 e2) = computeArithExpression evalE1 evalE2 op
                                                           where
                                                             evalE1 = evalExpression tEnvs vEnvs typeEnv e1
                                                             evalE2 = evalExpression tEnvs vEnvs typeEnv e2
                                                                             
--Function that computes an arithmetic operation
computeArithExpression :: Result -> Result -> ArithmeticOperator -> Result
computeArithExpression (ResInt n1) (ResInt n2) op = case op of
                                                     Add -> ResInt (n1 + n2)
                                                     Sub -> ResInt (n1-n2)
                                                     Mul -> ResInt (n1*n2)
                                                     Div -> ResInt (n1 `div` n2)
                                                     Exp -> ResInt (n1^n2)
                                                    
computeArithExpression (ResFloat f1) (ResFloat f2) op = case op of
                                                         Add -> ResFloat (f1+f2)
                                                         Sub -> ResFloat (f1-f2)
                                                         Mul -> ResFloat (f1*f2)
                                                         Div -> ResFloat (f1/f2)
                                                         Exp -> ResFloat (f1**f2)
                                                         
computeArithExpression (ResInt n1) (ResFloat f2) op = computeArithExpression (ResFloat (fromIntegral n1)) (ResFloat f2) op
computeArithExpression (ResFloat f1) (ResInt n2) op = computeArithExpression (ResFloat f1) (ResFloat (fromIntegral n2)) op

                                                         
computeArithExpression (ResInt n1) (ResQuery (head,t:tps,list)) op = case (extractUnique (head,t:tps,list)) of
                                                                       "" -> ResString ""
                                                                       xs -> case t of
                                                                                Int -> computeArithExpression (ResInt n1) (ResInt (read xs)) op
                                                                                Float -> computeArithExpression (ResFloat (fromIntegral n1)) (ResFloat (read xs)) op
                                                            
computeArithExpression (ResQuery (head,t:tps,list)) (ResInt n2) op = case (extractUnique (head,t:tps,list)) of
                                                                       "" -> ResString ""
                                                                       xs -> case t of
                                                                               Int -> computeArithExpression (ResInt (read xs)) (ResInt n2) op  
                                                                               Float -> computeArithExpression (ResFloat (read xs)) (ResFloat (fromIntegral n2)) op

computeArithExpression (ResFloat f1) (ResQuery tabList) op = case (extractUnique tabList) of   
                                                              "" -> ResString ""              
                                                              xs -> computeArithExpression (ResFloat f1) (ResFloat (read xs)) op
                                                              
computeArithExpression (ResQuery tabList) (ResFloat f2) op = case (extractUnique tabList) of  
                                                              "" -> ResString ""
                                                              xs -> computeArithExpression (ResFloat (read xs)) (ResFloat f2) op
                                                          
                                                         
computeArithExpression (ResQuery tabList1) (ResQuery tabList2) op = case (extractUnique tabList1, extractUnique tabList2) of
                                                                     ("",_) -> ResString ""
                                                                     (_,"") -> ResString ""
                                                                     (xs,ys) -> case (t1,t2) of
                                                                                  (Float,_) -> computeArithExpression (ResFloat (read xs)) (ResFloat (read ys)) op
                                                                                  (_,Float) -> computeArithExpression (ResFloat (read xs)) (ResFloat (read ys)) op
                                                                                  (Int,Int) -> computeArithExpression (ResInt (read xs)) (ResInt (read ys)) op
                                                                  where
                                                                    (_,t1:tps1,_) = tabList1
                                                                    (_,t2:tps2,_) = tabList2
                                                                    
computeArithExpression (ResString "") _ _ = ResString ""
computeArithExpression _ (ResString "") _ = ResString ""
                                                                    
--Function that evaluates columns for all rows in a table 
--       The fifth argument is all the rows of a table
evalColumnsRows :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [String] -> [Type] -> [[String]] -> Columns -> [([(String,Result)],[String])]
evalColumnsRows tEnvs vEnvs typeEnv heads types rows cols = evalColumnsRows' tEnvs vEnvs typeEnv heads types rows rows cols
                                                                  where
                                                                    evalColumnsRows' :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [String] -> [Type] -> [[String]] -> [[String]] -> Columns -> [([(String,Result)],[String])]
                                                                    evalColumnsRows' _ _ _ _ _ _ [] _ = []
                                                                    evalColumnsRows' tEnvs vEnvs typeEnv heads types allRows (row:rows) cols = (evalColumnsOneRow tEnvs vEnvs typeEnv heads types row allRows cols) : (evalColumnsRows' tEnvs vEnvs typeEnv heads types allRows rows cols)


--Function that evaluates columns for a single row
--      The third argument is the header of the table
--      The fourth argument is the types of the table
--      The fifth argument is the current row of the table
evalColumnsOneRow :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [String] -> [Type] -> [String] -> [[String]] -> Columns -> ([(String,Result)],[String])
evalColumnsOneRow tEnvs vEnvs typeEnv heads types row allRows (AllColumns) = ([],row)
evalColumnsOneRow tEnvs vEnvs typeEnv heads types row allRows (Cols cols) =  evalColumnsOneRow' tEnvs vEnvs typeEnv [] heads types row allRows [] cols
                                                                          where -- the last [String] is the accumulated string while [[String]] is all rows
                                                                            evalColumnsOneRow' :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [(String,Result)] -> [String] -> [Type] -> [String] -> [[String]] -> [String] -> [ColumnWithRename] -> ([(String,Result)],[String])
                                                                            evalColumnsOneRow' _ _ _ colEnvs _ _ _ _ accRow [] = (colEnvs,accRow)
                                                                            evalColumnsOneRow' tEnvs vEnvs typeEnv colEnvs heads types row allRows accRow (Col col ren : cols) = case ren of 
                                                                                                                                                                                   NoRename -> evalColumnsOneRow' tEnvs vEnvs typeEnv colEnvs heads types row allRows (accRow ++ [resultString]) cols
                                                                                                                                                                                   Rename xs -> evalColumnsOneRow' tEnvs vEnvs typeEnv ((xs,result):colEnvs)  heads types row allRows (accRow ++ [resultString]) cols
                                                                                                                                                                               where
                                                                                                                                                                                 result = evalColumn tEnvs vEnvs typeEnv colEnvs heads types row allRows col
                                                                                                                                                                                 resultString = castToString result
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
--Function that evaluates a column
--      The third argument is the  environment for the already computed Columns
--      The fourth argument is the headers of the table
--      The fifth argument is the type 
--      The sixth argument is the  current row
--      The eigth argument is all rows of the table which is needed for computing the aggregate functions
--      The ninth argument is the current column to evaluate
--
evalColumn :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [(String,Result)] -> [String] -> [Type] -> [String] -> [[String]] -> Column -> Result
evalColumn _ _ _ colEnvs heads types row _ (ColIndex (ColName xs)) = case lookup xs colEnvs of
                                                                             Just r -> r
                                                                             Nothing -> case lookup xs zipped of
                                                                                          Just (t,v) -> castFromString t v
                                                                                          Nothing -> error ("The column " ++ xs ++ " does not exist")
                                                                         where
                                                                           zipped = zip heads (zip types row)
                                                            
evalColumn _ _ _ _ _ types row _ (ColIndex (ColInt n)) = castFromString (types !! (n-1)) (row !! (n-1))

evalColumn tEnvs vEnvs typeEnv _ _ _ _ _ (ColExpr e) = case evalExpression tEnvs vEnvs typeEnv e of
                                                        ResQuery (head,t:tps,tabList) -> castFromString t (extractUnique (head,t:tps,tabList))
                                                        result -> result
                                                 
evalColumn tEnvs vEnvs typeEnv colEnvs heads types row allRows (ColOp biOp c1 c2) = case biOp of
                                                                                BiLogOp op -> computeLogicBoolExpression  evalC1 evalC2 op
                                                                                BiCompOp op -> computeCompBoolExpression evalC1 evalC2 op
                                                                                BiArithOp op -> computeArithExpression evalC1 evalC2 op
                                                                                StrOp op -> computeStrExpression evalC1 evalC2 op
                                                                      
                                                                           where
                                                                             evalC1 = evalColumn tEnvs vEnvs typeEnv colEnvs heads types row allRows c1
                                                                             evalC2 = evalColumn tEnvs vEnvs typeEnv colEnvs heads types row allRows c2
                                                                             
evalColumn tEnvs vEnvs typeEnv colEnvs heads types row allRows (ColAggregation aggreFun (ColInt n)) = case aggreFun of
                                                                                                        Sum -> castFromString t (sumAggregation t columnAggregate)
                                                                                                        Count -> castFromString Int (countAggregation columnAggregate)
                                                                                                        Avg -> castFromString Float (avgAggregation t columnAggregate)
                                                                                                        Min -> castFromString t (minAggregation t columnAggregate)
                                                                                                        Max -> castFromString t (maxAggregation t columnAggregate)
                                                                                                    where
                                                                                                        t = types !! (n-1)
                                                                                                        columnAggregate = map (!! (n-1)) allRows
                                                                                                        
evalColumn tEnvs vEnvs typeEnv colEnvs heads types row allRows (ColAggregation aggreFun (ColName xs)) = case lookup xs zipped of
                                                                                                          Just n -> evalColumn tEnvs vEnvs typeEnv colEnvs heads types row allRows (ColAggregation aggreFun (ColInt n))
                                                                                                          Nothing -> error $ "Column " ++ xs ++ " does not exist or cannot be used to perform aggregation"
                                                                                                      where
                                                                                                        zipped = zip heads [1..] 
                                                                                                        
evalColumn tEnvs vEnvs typeEnv colEnvs heads types row allRows (ColIfThenElse rowFs col1 col2) = if resultIfThenElse then fstCol else sndCol
                                                                                               where
                                                                                                 resultIfThenElse = computeRowFilters tEnvs vEnvs typeEnv colEnvs heads types row allRows rowFs
                                                                                                 fstCol = evalColumn tEnvs vEnvs typeEnv colEnvs heads types row allRows col1
                                                                                                 sndCol = evalColumn tEnvs vEnvs typeEnv colEnvs heads types row allRows col2
                                                                                                        

--Function that computes the row filters
computeRowFilters :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [(String,Result)] -> [String] -> [Type] -> [String] -> [[String]] -> RowFilters -> Bool
computeRowFilters tEnvs vEnvs typeEnv colEnvs head types row allRows (RowFilterNot rowFilt) = not $ computeRowFilters tEnvs vEnvs typeEnv colEnvs head types row allRows rowFilt

computeRowFilters tEnvs vEnvs typeEnv colEnvs head types row allRows (RowFilter rowFilt) = case rowFilt of
                                                                                     Empty cols -> emptyCols colEnvs head row cols
                                                                                     NotEmpty cols -> notEmptyCols colEnvs head row cols
                                                                                     RowComparison compOp c1 c2 -> rowComparison tEnvs vEnvs typeEnv colEnvs head types row allRows c1 compOp c2
                                                                                     RowSats compOp c1 cols -> rowSats tEnvs vEnvs typeEnv colEnvs head types row allRows c1 compOp cols
                                                                                     RowBetw c1 c2 c3 -> rowBetw tEnvs vEnvs typeEnv colEnvs head types row  allRows c1 c2 c3
                                                                                     RowNotBetw c1 c2 c3 -> rowNotBetw tEnvs vEnvs typeEnv colEnvs head types row allRows c1 c2 c3
                                                                                     RowIfThenElse rowFs fstF sndF -> if resultRowFs then resultFstF else resultSndF 
                                                                                        where
                                                                                          resultRowFs = computeRowFilters tEnvs vEnvs typeEnv colEnvs head types row allRows rowFs
                                                                                          resultFstF = computeRowFilters tEnvs vEnvs typeEnv colEnvs head types row allRows (RowFilter fstF)
                                                                                          resultSndF = computeRowFilters tEnvs vEnvs typeEnv colEnvs head types row allRows (RowFilter sndF)
                                                                             
computeRowFilters tEnvs vEnvs typeEnv colEnvs head types row allRows (RowFilters biLogOp rowFilt1 rowFilt2) = case biLogOp of
                                                                                                                And -> evalRowFilt1 && evalRowFilt2
                                                                                                                Or -> evalRowFilt1 || evalRowFilt2    
                                                                                                            where
                                                                                                             evalRowFilt1 = computeRowFilters tEnvs vEnvs typeEnv colEnvs head types row allRows rowFilt1
                                                                                                             evalRowFilt2 = computeRowFilters tEnvs vEnvs typeEnv colEnvs head types row allRows rowFilt2
                                                                             

--Function that computes the empty operator for a list of columns
--      The first argument is the  environment for the already computed Columns
--      The second argument is the headers of the table
--      The third argument is the  current row
--      The fourth argument is the columns to check for emptyness
--
emptyCols :: [(String,Result)] -> [String] -> [String] -> [ColIndex] -> Bool
emptyCols colEnvs head row cols = and $ map (emptyCol colEnvs head row) cols


--Function that computes the empty operator for one column
--      The first argument is the  environment for the already computed Columns
--      The second argument is the headers of the table
--      The third argument is the  current row
--      The fourth argument is the column to check for emptyness
--
emptyCol :: [(String,Result)] -> [String] -> [String] -> ColIndex -> Bool
emptyCol _ _ row (ColInt n) = (row !! (n-1)) == ""
emptyCol colEnvs head row (ColName ys) = case lookup ys colEnvs of
                                         Just (ResString "") -> True
                                         Just _ -> False
                                         Nothing -> case lookup ys zipped of
                                                      Just "" -> True
                                                      Just _ -> False
                                                      Nothing -> error ("The column " ++ ys ++ " does not exist")
                                         where
                                          zipped = zip head row
                                          
--Function that computes the notempty operator for a list of columns
--      The first argument is the  environment for the already computed Columns
--      The second argument is the headers of the table
--      The third argument is the  current row
--      The fourth argument is the columns to check for non-emptyness
--
notEmptyCols :: [(String,Result)] -> [String] -> [String] -> [ColIndex] -> Bool
notEmptyCols colEnvs head row cols = and $ map (not . (emptyCol colEnvs head row)) cols 


--Function that compares two columns in a row
--      The third argument is the  environment for the already computed Columns
--      The fourth argument is the headers of the table
--      The fifth argument is the type 
--      The sixth argument is the current row
--      The seventh argument is all the rows of the table
--      The seventh argument is the first column to compare
--      The eight argument is the comparison operator
--      The ninth argument is the second column to compare
rowComparison :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [(String,Result)] -> [String] -> [Type] -> [String] -> [[String]] -> Column -> ComparisonOperator -> Column -> Bool
rowComparison tEnvs vEnvs typeEnv colEnvs head types row allRows c1 compOp c2 = case computeCompBoolExpression evalC1 evalC2 compOp of
                                                                                  ResBool bool -> bool
                                                                                  ResString "" -> False
                                                                             where
                                                                               evalC1 = evalColumn tEnvs vEnvs typeEnv colEnvs head types row allRows c1
                                                                               evalC2 = evalColumn tEnvs vEnvs typeEnv colEnvs head types row allRows c2


--Function that checks whether a comparison between one column and another column holds given a list of columns
--      The third argument is the  environment for the already computed Columns
--      The fourth argument is the headers of the table
--      The fifth argument is the type 
--      The sixth argument is the  current row
--      The seventh argument is all rows
--      The eigth argument is the first column to compare
--      The nineth argument is the list of columns to compare
rowSats :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [(String,Result)] -> [String] -> [Type] -> [String] -> [[String]] -> Column -> ComparisonOperator -> [Column] -> Bool
rowSats tEnvs vEnvs typeEnv colEnvs head types row allRows c1 comOp cols = or $ map (rowComparison tEnvs vEnvs typeEnv colEnvs head types row allRows c1 comOp) cols
                                                            

--Function that checks whether a column c1 is between c2 and c3
--      The third argument is the  environment for the already computed Columns
--      The fourth argument is the headers of the table
--      The fifth argument is the type 
--      The sixth argument is the  current row
--      The seventh argument is all rows
--      The eigth argument is the first column c1 to checks whether it is between c2 and c3
--      The nineth argument is the column c2
--      The tenth argument is the column c3
rowBetw :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [(String,Result)] -> [String] -> [Type] -> [String] -> [[String]] -> Column -> Column -> Column -> Bool
rowBetw tEnvs vEnvs typeEnv colEnvs head types row allRows c1 c2 c3 = (rowComparison tEnvs vEnvs typeEnv colEnvs head types row allRows c1 GreaterThanEq c2) && (rowComparison tEnvs vEnvs typeEnv colEnvs head types row allRows c1 LessThanEq c3)


--Function that checks whether a column c1 is not between c2 and c3
--      The third argument is the  environment for the already computed Columns
--      The fourth argument is the headers of the table
--      The fifth argument is the type 
--      The sixth argument is the  current row
--      The seventh argument is all rows
--      The seventh argument is the first column to checks whether it is between c2 and c3
--      The eight argument is the column c2
--      The ninth argument is the column c3
rowNotBetw :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [(String,Result)] -> [String] -> [Type] -> [String] -> [[String]] -> Column -> Column -> Column -> Bool
rowNotBetw tEnvs vEnvs typeEnv colEnvs head types row allRows c1 c2 c3 = not $ rowBetw tEnvs vEnvs typeEnv colEnvs head types row allRows c1 c2 c3

                                                                                                                
--Function that casts a string to a result given a type
castFromString :: Type -> String -> Result
castFromString Int xs = case xs of
                          "" -> ResString ""
                          _ -> ResInt (read xs)
                          
castFromString Float xs = case xs of
                            "" -> ResString ""
                            _ -> ResFloat (read xs)
                            
castFromString String xs  = ResString xs

castFromString Bool xs = case xs of
                           "" -> ResString ""
                           _ -> ResBool (read xs)

--Function that casts a Result to a String
castToString :: Result -> String
castToString (ResBool b) = show b
castToString (ResInt n) = show n
castToString (ResFloat f) = show f
castToString (ResString xs) = xs
                                                          
                                                                                                                                      
--Function that extracts a unique element in a table
extractUnique :: TableList -> String
extractUnique (_,_,[]) = throw NotSingletonTableNoRowException
extractUnique (_,_,tabList) | length tabList > 1 || length (head tabList) > 1 = throw NotSingletonTableMultipleRowsException
                            | otherwise = head $ head tabList                                                           
 
--Function that groups by all rows in a table 
--   It outputs all rows groupped according to the list of the group by columns
groupByRows :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [[(String,Result)]] -> [String] -> [Type] -> [[String]] -> [Column] -> [[[String]]]
groupByRows tEnvs vEnvs typeEnv colEnvs head types rows colGroups = groupByRows' [] tEnvs vEnvs typeEnv colEnvs head types rows rows colGroups
                                                                  where
                                                                    groupByRows' :: [([Result],[[String]])] -> TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [[(String,Result)]] -> [String] -> [Type] -> [[String]] -> [[String]] -> [Column] -> [[[String]]]
                                                                    groupByRows' acc _ _ _ _ _ _ _ [] _ = map snd acc
                                                                    groupByRows' acc tEnvs vEnvs typeEnv [] head types allRows (roww:rowss) colGroups = case lookup results acc of
                                                                                                                                                                        Just rows -> groupByRows' ( (results, (row:rows)) : (delete (results,rows) acc)) tEnvs vEnvs typeEnv [] head types allRows rowss colGroups
                                                                                                                                                                        Nothing -> groupByRows' ((results,[row]):acc) tEnvs vEnvs typeEnv [] head types allRows rowss colGroups
                                                                                                                                                    where
                                                                                                                                                      (results, row) = groupByOneRow tEnvs vEnvs typeEnv [] head types roww allRows colGroups
                                                                                                                                                                     
                                                                    groupByRows' acc tEnvs vEnvs typeEnv (colEnv : colEnvs) head types allRows (roww:rowss) colGroups = case lookup results acc of
                                                                                                                                                                        Just rows -> groupByRows' ( (results, (row:rows)) : (delete (results,rows) acc)) tEnvs vEnvs typeEnv colEnvs head types allRows rowss colGroups
                                                                                                                                                                        Nothing -> groupByRows' ((results,[row]):acc) tEnvs vEnvs typeEnv colEnvs head types allRows rowss colGroups
                                                                                                                                                                    where
                                                                                                                                                                     (results, row) = groupByOneRow tEnvs vEnvs typeEnv colEnv head types roww allRows colGroups

--Function group by one row
--      The third argument is the  environment for the new columns created inside the get
--      The fourth argument is the headers of the table
--      The fifth argument is the type 
--      The sixth argument is the current row
--      The seventh argument is all rows
--      The eighth argument is the list of columns to group by
groupByOneRow :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [(String, Result)] -> [String] -> [Type] -> [String] -> [[String]] -> [Column] -> ([Result],[String])
groupByOneRow tEnvs vEnvs typeEnv colEnvs head types row allRows colGroups = (map (evalColumn tEnvs vEnvs typeEnv colEnvs head types row allRows) colGroups, row)


--Function that order the rows of a table according to a list of columns
--      The third argument is the  environment for the new columns created inside the get
--      The fourth argument is the headers of the table
--      The fifth argument is the type 
--      The sixth argument is the  current rows in the table
--      The seventh argument is the rows that should be output
--      The eight argument is the list of columns to order by
--      The ninth argument is the order
orderByRows :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [[(String,Result)]] -> [String] -> [Type] -> [[String]] -> [[String]] -> [Column] -> Order -> [[String]]
orderByRows tEnvs vEnvs typeEnv colEnvs head types rows rowsToOutput colOrder ord | ord == Asc = orderedRows
                                                                                  | ord == Desc = reverse orderedRows
                                                                                  where
                                                                                    orderByRows' :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [[(String,Result)]] -> [String] -> [Type] -> [[String]] -> [[String]] -> [[String]] -> [Column] -> [([Result],[String])]
                                                                                    orderByRows' _ _ _ [] _ _ _ _ _ _  = []
                                                                                    orderByRows' tEnvs vEnvs typeEnv (colEnv : colEnvs) head types (row : rows) (rowToOutput : rowsToOutput) allRows colOrder = (orderByOneRow tEnvs vEnvs typeEnv colEnv head types row rowToOutput allRows colOrder) : (orderByRows' tEnvs vEnvs typeEnv colEnvs head types rows rowsToOutput allRows colOrder)
                                                                        
                                                                                    orderedRows = map snd $ sort $ orderByRows' tEnvs vEnvs typeEnv colEnvs head types rows rowsToOutput rows colOrder


--Function that given a row and the columns to order by, it attaches the row to output with a list of Result values which will be used to order by
--      The third argument is the  environment for the new columns created inside the get
--      The fourth argument is the headers of the table
--      The fifth argument is the type 
--      The sixth argument is the  current row in the table
--      The seventh argument is the row that should be output
--      The eight argument is all rows
--      The nineth argument is the list of columns to order by
orderByOneRow :: TableEnvironment -> VariableEnvironment -> TypeEnvironment -> [(String,Result)] -> [String] -> [Type] -> [String] -> [String] -> [[String]] -> [Column] -> ([Result],[String])
orderByOneRow tEnvs vEnvs typeEnv colEnvs head types row rowToOutput allRows colOrder = (map (evalColumn tEnvs vEnvs typeEnv colEnvs head types row allRows ) colOrder, rowToOutput)

----------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------TYPE CHECKER-----------------------------------------------------------------------------
{-typeCheck :: Program -> IO Bool
typeCheck p = do
                types <- typeCheck' [] 1 p
                --if length p == length types then return True else return False
                return $ checkTypes types
            where
              typeCheck' :: TypeEnvironment -> Int -> Program -> IO [Type]
              typeCheck' _ _ [] = return []
              typeCheck' tEnv n ((Import _ types header _ tName):ps) | length header == length types = do
                                                                                                         restTypes <- catch (typeCheck' ((tName,(Table header types)): tEnv) (n+1) ps) (\e -> throwTypeCheckerException n e)
                                                                                                         return (Void : restTypes)    
                                                                     | otherwise = throw (TypeCheckerException n (HeaderException tName header types))
                                                                            
              typeCheck' tEnv n ((VarAssign varName e):ps) = do
                                                               restTypes <- catch (typeCheck' ((varName,typeOfExpression tEnv e):tEnv) (n+1) ps) (\e -> throwTypeCheckerException n e)
                                                               return (Void : restTypes)
              typeCheck' tEnv n ((S query):ps) = do
                                                   restTypes <- catch (typeCheck' tEnv (n+1) ps) (\e -> throwTypeCheckerException n e)
                                                   return $ (typeOfQuery tEnv query) : restTypes -}
                                                   
typeCheck :: Program -> IO Bool
typeCheck p = do
                types <- typeCheck' [] 1 p
                --if length p == length types then return True else return False
                return True
            where
              typeCheck' :: TypeEnvironment -> Int -> Program -> IO [Type]
              typeCheck' _ _ [] = return []
              typeCheck' tEnv n ((Import _ types header _ tName):ps) | length header == length types = do
                                                                                                         restTypes <- catch (typeCheck' ((tName,(Table header types)): tEnv) (n+1) ps) (\e -> throwTypeCheckerException n e)
                                                                                                         return (Void : restTypes)    
                                                                     | otherwise = throw (TypeCheckerException n (HeaderException tName header types))
                                                                            
              typeCheck' tEnv n ((VarAssign varName e):ps) = catch (do
                                                               let typeExpression = typeOfExpression tEnv e
                                                               case checkType typeExpression of
                                                                 True -> do
                                                                           restTypes <- typeCheck' ((varName,typeOfExpression tEnv e):tEnv) (n+1) ps 
                                                                           return (Void : restTypes)) (\e -> throwTypeCheckerException n e)
              typeCheck' tEnv n ((S query):ps) = catch (do
                                                   let typeQuery = typeOfQuery tEnv query
                                                   case checkType typeQuery of
                                                     True -> do
                                                                restTypes <- typeCheck' tEnv (n+1) ps
                                                                return (typeQuery : restTypes)) (\e -> throwTypeCheckerException n e)         
                                                                
                                                   
checkTypes :: [Type] -> Bool
checkTypes [] = True
checkTypes (x:xs) = case x of
                      Int -> checkTypes xs
                      Bool -> checkTypes xs
                      Float -> checkTypes xs
                      String -> checkTypes xs
                      Void -> checkTypes xs
                      Table _ types -> if checkTypes types then checkTypes xs else False
                      
checkType :: Type -> Bool
checkType x = case x of
                      Int -> True
                      Bool -> True
                      Float -> True
                      String -> True
                      Void -> True
                      Table _ types -> if checkTypes types then True else False                      
                      
                      
--Function that throws a type checker exception
throwTypeCheckerException n (TypeCheckerException n2 e) = throw (TypeCheckerException n2 e)
throwTypeCheckerException n e = throw (TypeCheckerException n e)

-- Function that computes the type of a program
typeOfProgram :: Program -> [Type]
typeOfProgram p = typeOfProgram' [] p
                where
                  typeOfProgram' :: TypeEnvironment -> Program -> [Type]
                  typeOfProgram' _ [] = []
                  typeOfProgram' tEnv ((Import _ types header _ tName):ps) | length header == length types =  Void : typeOfProgram' ((tName,(Table header types)): tEnv) ps    
                                                                           | otherwise = throw (HeaderException tName header types)
                                                                            
                  typeOfProgram' tEnv ((VarAssign varName e):ps) = Void : typeOfProgram' ((varName,typeOfExpression tEnv e):tEnv) ps
                  typeOfProgram' tEnv ((S query):ps) = (typeOfQuery tEnv query) : typeOfProgram' tEnv ps


--Function that computes the type of a query
typeOfQuery :: TypeEnvironment -> Query -> Type
typeOfQuery tEnv (Get _ (TName name) cols conds) = case lookup name tEnv of
                                                     Just (Table head types) -> let (newHead, newTypes) = unzip $ typeOfCols tEnv head types cols in let colEnv = (zip newHead newTypes) \\ (zip head types) in  case checkGetConditionsWellTyped tEnv colEnv head types conds of
                                                                                                                                                                                                                    True -> Table newHead newTypes
                                                                                  
                                                     Nothing -> throw (NonExistingTableException name)
                                                            
                                                        
typeOfQuery tEnv (Get _ (TQuery q) cols conds) = case checkGetConditionsWellTyped tEnv colEnv head types conds of
                                                   True -> Table newHead newTypes
                                               where
                                                 Table head types = typeOfQuery tEnv q
                                                 (newHead, newTypes) = unzip $ typeOfCols tEnv head types cols
                                                 colEnv = (zip newHead newTypes) \\ (zip head types)
                                                        
typeOfQuery tEnv (AggreQuery aggreFun query) | tps == [] = Table [h] [computeTypeAggregation t aggreFun]
                                             | otherwise = throw (NonSingletonTableInAggregation aggreFun (Table (h:hs) (t:tps) ))
                                             where
                                                Table (h:hs) (t:tps) = typeOfQuery tEnv query
                                                
typeOfQuery tEnv (Union q1 q2) | types1 == types2 = Table head1 types1
                               | otherwise = throw (UnionException (Table head1 types1) (Table head2 types2))
                               where
                                 Table head1 types1 = typeOfQuery tEnv q1
                                 Table head2 types2 = typeOfQuery tEnv q2
                                 
typeOfQuery tEnv (Merge q1 q2) | types1 == types2 = Table head1 types1
                               | otherwise = throw (MergeException (Table head1 types1) (Table head2 types2))
                               where
                                 Table head1 types1 = typeOfQuery tEnv q1
                                 Table head2 types2 = typeOfQuery tEnv q2 
                                 
typeOfQuery tEnv (VarQuery varName) = case lookup varName tEnv of
                                        Just (Table head types) -> Table head types
                                        Just t -> throw (NonQueryVariable varName t)
                                        Nothing -> throw (NonExistingQueryVariable varName)
                                        
typeOfQuery tEnv (ProdQuery prd q1 q2 conds) = case checkGetConditionsWellTyped  tEnv [] (head1++head2) (types1++types2) (Just conds,Nothing) of
                                                   True -> Table (head1 ++ head2) (types1 ++ types2)
                                               where
                                                 Table head1 types1 = typeOfQuery tEnv q1
                                                 Table head2 types2 = typeOfQuery tEnv q2
                                                                                       

--Function that computes the type of columns
--      The second argument is the header of the table
--      The third argument is the types of the header
typeOfCols :: TypeEnvironment -> [String] -> [Type] -> Columns -> [(String,Type)] 
typeOfCols tEnv head types AllColumns = zip head types
                                      
typeOfCols tEnv head types (Cols columns) = typeOfCols' tEnv [] head types columns
                                          where
                                            typeOfCols' :: TypeEnvironment -> ColumnTypeEnvironment -> [String] -> [Type] -> [ColumnWithRename] -> [(String,Type)]
                                            typeOfCols' _ _ _ _ [] = []
                                            typeOfCols' tEnv colTEnv head types ((Col col NoRename):cols) = (computeColName colNames head (Col col NoRename),typeOfCol tEnv colTEnv head types col) : typeOfCols' tEnv colTEnv head types cols
                                                                                                          where
                                                                                                            (colNames, ts) = unzip tEnv
                                                                                                            
                                            typeOfCols' tEnv colTEnv head types ((Col col (Rename xs)):cols) = (computeColName colNames head (Col col (Rename xs)),typeCol) : typeOfCols' tEnv ((xs,typeCol):colTEnv) head types cols
                                                                                                             where
                                                                                                               typeCol = (typeOfCol tEnv colTEnv head types col)
                                                                                                               (colNames, ts) = unzip tEnv
                                                                                                              
--Function that computes the name of the column 
--       The first argument is the new columns added inside the GET
--       The second argument is the header  of the table
computeColName :: [String] -> [String] -> ColumnWithRename -> String 
computeColName cols head (Col _ (Rename xs)) = xs    
computeColName cols head (Col (ColIndex (ColInt n)) _) = case lookup n zipped of
                                                           Just xs -> xs
                                                           Nothing -> throw (NonExistingColumnInt n)
                                                       where
                                                         zipped = zip [1..] head
                                                         
computeColName cols head (Col (ColIndex (ColName xs)) _) | elem xs cols = throw (DuplicatedColumnInGetException xs)
                                                         | elem xs head = xs
                                                         | otherwise = throw (NonExistingColumnName xs)  

computeColName _ _ _ = ""                                                         
                                                         
                                                         
--Function that computes the type of a column
--      The second argument is the type environment of the new columns being created inside the query
--      The third argument is the header of the table
--      The fourth argument is the types of the header
typeOfCol :: TypeEnvironment -> ColumnTypeEnvironment -> [String] -> [Type] -> Column -> Type
typeOfCol _ _ _ types (ColIndex (ColInt n)) = case lookup n zipped of
                                           Just t -> t
                                           Nothing -> throw (NonExistingColumnInt n)
                                           where
                                             zipped = zip [1..] types
                                         
typeOfCol tEnv colTEnv head types (ColIndex (ColName xs)) = case lookup xs colTEnv of
                                                              Just t -> t
                                                              Nothing -> case lookup xs zipped of
                                                                           Just t -> t
                                                                           Nothing -> throw (NonExistingColumnName xs)
                                                          where
                                                            zipped = zip head types
                                                            
typeOfCol tEnv _ _ _ (ColExpr e) = case typeOfExpression tEnv e of
                                     Table _ [t] -> t
                                     Table head (t:ts) -> throw (NonSingletonTableInExpression (Table head (t:ts)))
                                     t -> t
                                     
typeOfCol tEnv colTEnv head types (ColOp biOp c1 c2) = case biOp of
                                                         BiLogOp biLogOp -> computeTypeBinaryLogicOp evalC1 evalC2 biLogOp
                                                         BiCompOp biCompOp -> computeTypeComparisonOp evalC1 evalC2 biCompOp
                                                         BiArithOp biArithOp -> computeTypeArithOp evalC1 evalC2 biArithOp
                                                         StrOp strOp -> computeTypeStrOp evalC1 evalC2 strOp
                                                     where
                                                       evalC1 = typeOfCol tEnv colTEnv head types c1
                                                       evalC2 = typeOfCol tEnv colTEnv head types c2
                                                       
typeOfCol tEnv colTEnv head types (ColAggregation aggreFun (ColInt n)) = case lookup n zipped of
                                                                           Just t -> computeTypeAggregation t aggreFun
                                                                           Nothing -> throw (NonExistingColumnInt n)
                                                                       where
                                                                         zipped = zip [1..] types
                                                                         
typeOfCol tEnv colTEnv head types (ColAggregation aggreFun (ColName xs)) = case lookup xs colTEnv of
                                                                             Just t -> computeTypeAggregation t aggreFun
                                                                             Nothing -> case lookup xs zipped of
                                                                                          Just t -> computeTypeAggregation t aggreFun
                                                                                          Nothing -> throw (NonExistingColumnName xs)
                                                                         where
                                                                            zipped = zip head types
                                                                            
typeOfCol tEnv colTEnv head types (ColIfThenElse rowFs col1 col2) | wellTypedRowFs && (typeOfCol1 == typeOfCol2) = typeOfCol1
                                                                  | otherwise = throw (IncorrectArgsIfThenElseCol typeOfCol1 typeOfCol2)
                                                                  where
                                                                    wellTypedRowFs = checkRowFiltersWellTyped tEnv colTEnv head types rowFs
                                                                    typeOfCol1 = typeOfCol tEnv colTEnv head types col1
                                                                    typeOfCol2 = typeOfCol tEnv colTEnv head types col2
                                                                    
--Function that given an aggregation function and the type of the column where the function is applied returns the type
computeTypeAggregation :: Type -> AggregateFunction -> Type
computeTypeAggregation _ Count = Int
computeTypeAggregation Int Sum = Int
computeTypeAggregation Float Sum = Float
computeTypeAggregation Int Avg = Float
computeTypeAggregation Float Avg = Float
computeTypeAggregation Int Min = Int 
computeTypeAggregation Float Min = Float
computeTypeAggregation Bool Min = Bool
computeTypeAggregation String Min = String
computeTypeAggregation t Max = computeTypeAggregation t Min
computeTypeAggregation t aggreFun = throw (WrongColumnTypeInAggregation aggreFun t)

                                                       
--Function that computes the type of an expression
typeOfExpression :: TypeEnvironment -> Expression -> Type
typeOfExpression tEnv (EString strExpression) = typeOfStrExpression tEnv strExpression
typeOfExpression tEnv (EBool boolExpression) = typeOfBoolExpression tEnv boolExpression
typeOfExpression tEnv (EArith arithExpression) = typeOfArithExpression tEnv arithExpression
typeOfExpression tEnv (EVar var) = case lookup var tEnv of
                                         Just t -> t
                                         Nothing -> throw (NonExistingVariable var)
                                         
typeOfExpression tEnv (EQuery query) = typeOfQuery tEnv query

--Function that computes the type of a string expression
typeOfStrExpression :: TypeEnvironment -> StringExpression -> Type
typeOfStrExpression tEnv (StringLit _) = String
typeOfStrExpression tEnv (StringOp strOp e1 e2) = computeTypeStrOp (typeOfExpression tEnv e1) (typeOfExpression tEnv e2) strOp
                                                
--Function that computes the type of a boolean expression
typeOfBoolExpression :: TypeEnvironment -> BoolExpression -> Type
typeOfBoolExpression tEnv BoolTrue = Bool
typeOfBoolExpression tEnv BoolFalse = Bool
typeOfBoolExpression tEnv (BoolNot e) = computeTypeUnaryLogicOp (typeOfExpression tEnv e) Not
                                      
typeOfBoolExpression tEnv (BoolOpComp compOp e1 e2) = computeTypeComparisonOp (typeOfExpression tEnv e1) (typeOfExpression tEnv e2) compOp
                                                    
typeOfBoolExpression tEnv (BoolOpLogic biLogOp e1 e2) = computeTypeBinaryLogicOp (typeOfExpression tEnv e1) (typeOfExpression tEnv e2) biLogOp
                                                      
                                                      
--Function that computes the type of an arithmetic expression
typeOfArithExpression :: TypeEnvironment -> ArithExpression -> Type
typeOfArithExpression tEnv (IntLit _) = Int
typeOfArithExpression tEnv (FloatLit _) = Float
typeOfArithExpression tEnv (ArithOp arithOp e1 e2) = computeTypeArithOp (typeOfExpression tEnv e1) (typeOfExpression tEnv e2) arithOp

--Function that computes the type given a string operator and two types    
computeTypeStrOp :: Type -> Type -> StringOperator -> Type 
computeTypeStrOp String String strOp = String     
computeTypeStrOp t1 t2 strOp = throw (IncorrectArgsStringOperatorException strOp t1 t2) 
                                         
--Function that computes the type given a binary logic operator and two types
computeTypeBinaryLogicOp :: Type -> Type -> BinaryLogicOperator -> Type
computeTypeBinaryLogicOp Bool Bool _ = Bool
computeTypeBinaryLogicOp t1 t2 biLogOp = throw (IncorrectArgsBiLogicOpException biLogOp t1 t2)  

--Function that computes the type given a comparison operator and two types  
computeTypeComparisonOp :: Type -> Type -> ComparisonOperator -> Type  
computeTypeComparisonOp  t1 t2 compOp | (t1 == t2) || ((t1 == Int || t1 == Float) && (t2 == Float || t2 == Int)) = Bool    
                                      | otherwise =  throw (IncorrectArgsCompOpException compOp t1 t2)      

--Function that computes the type given a unary logic operator and one type                                 
computeTypeUnaryLogicOp :: Type -> UnaryLogicOperator -> Type  
computeTypeUnaryLogicOp t1 unOp | t1 == Bool = Bool
                                | otherwise =  throw (IncorrectArgUnaryLogicOpException unOp t1 )  
                                
--Function that computes the type given an arithmetic operator and two types
computeTypeArithOp :: Type -> Type -> ArithmeticOperator -> Type
computeTypeArithOp Int Int _ = Int
computeTypeArithOp Float Int _ = Float
computeTypeArithOp Int Float _ = Float
computeTypeArithOp Float Float _ = Float
computeTypeArithOp t1 t2 arithOp = throw (IncorrectArgsArithOpException arithOp t1 t2)


--Function that checks whether the getConditions are well typed
checkGetConditionsWellTyped ::  TypeEnvironment -> ColumnTypeEnvironment -> [String] -> [Type] -> GetConditions -> Bool
checkGetConditionsWellTyped _ _ _ _ (Nothing,Nothing) = True
checkGetConditionsWellTyped tEnv colEnv head types (Just rowFilts, Nothing) = checkRowFiltersWellTyped tEnv colEnv head types rowFilts
checkGetConditionsWellTyped tEnv colEnv head types (Nothing, Just grOrdLimits) = checkGroupOrderLimitsWellTyped tEnv colEnv head types grOrdLimits
checkGetConditionsWellTyped tEnv colEnv head types (Just rowFilts, Just grOrdLimits) = (checkRowFiltersWellTyped tEnv colEnv head types rowFilts) && (checkGroupOrderLimitsWellTyped tEnv colEnv head types grOrdLimits)


--Function that checks whether row filters are well typed
checkRowFiltersWellTyped :: TypeEnvironment -> ColumnTypeEnvironment -> [String] -> [Type] -> RowFilters -> Bool
checkRowFiltersWellTyped tEnv colEnv head types (RowFilter rowF) = checkRowFilterWellTyped tEnv colEnv head types rowF
checkRowFiltersWellTyped tEnv colEnv head types (RowFilterNot rowF) = checkRowFiltersWellTyped tEnv colEnv head types rowF
checkRowFiltersWellTyped tEnv colEnv head types (RowFilters _ rowF1 rowF2) = checkRowFiltersWellTyped tEnv colEnv head types rowF1 && checkRowFiltersWellTyped tEnv colEnv head types rowF2

                                                   
--Function that checks whether a row filter is well typed
checkRowFilterWellTyped :: TypeEnvironment -> ColumnTypeEnvironment -> [String] -> [Type] -> RowFilter -> Bool
checkRowFilterWellTyped tEnv colEnv head types (Empty cols) | length ( map (typeOfCol tEnv colEnv head types) (map ColIndex cols)) == length cols = True
                                                                
checkRowFilterWellTyped tEnv colEnv head types (NotEmpty cols) = checkRowFilterWellTyped tEnv colEnv head types (Empty cols)
checkRowFilterWellTyped tEnv colEnv head types (RowComparison compOp c1 c2) = case computeTypeComparisonOp evalC1 evalC2 compOp of
                                                                                Bool -> True
                                                                            where
                                                                              evalC1 = typeOfCol tEnv colEnv head types c1
                                                                              evalC2 = typeOfCol tEnv colEnv head types c2
                                                                              
checkRowFilterWellTyped tEnv colEnv head types (RowSats compOp c1 cols) = and $ map (checkRowFilterWellTyped tEnv colEnv head types) (map (RowComparison compOp c1) cols)

checkRowFilterWellTyped tEnv colEnv head types (RowBetw c1 c2 c3) = checkRowFilterWellTyped tEnv colEnv head types (RowComparison GreaterThanEq c1 c2) && checkRowFilterWellTyped tEnv colEnv head types (RowComparison LessThanEq c1 c3)
checkRowFilterWellTyped tEnv colEnv head types (RowNotBetw c1 c2 c3) = checkRowFilterWellTyped tEnv colEnv head types (RowBetw c1 c2 c3)
checkRowFilterWellTyped tEnv colEnv head types (RowIfThenElse rowFs rowF1 rowF2) = resultRowFs && resultRowF1 && resultRowF2
                                                                                  where
                                                                                    resultRowFs = checkRowFiltersWellTyped tEnv colEnv head types rowFs
                                                                                    resultRowF1 = checkRowFilterWellTyped tEnv colEnv head types rowF1
                                                                                    resultRowF2 = checkRowFilterWellTyped tEnv colEnv head types rowF2
--Function that checks whether a list of groupOrder limits is well typed
checkGroupOrderLimitsWellTyped :: TypeEnvironment -> ColumnTypeEnvironment -> [String] -> [Type] -> [GroupOrderLimit] -> Bool
checkGroupOrderLimitsWellTyped tEnv colEnv head types grOrdLimits = and $ map (checkGroupOrderLimitWellTyped tEnv colEnv head types) grOrdLimits

--Function that checks whether a groupOrder limit is well typed 
checkGroupOrderLimitWellTyped :: TypeEnvironment -> ColumnTypeEnvironment -> [String] -> [Type] -> GroupOrderLimit -> Bool
checkGroupOrderLimitWellTyped tEnv colEnv head types (GroupBy cols) | length  (map (typeOfCol tEnv colEnv head types) cols) == length cols = True
                                                                          
checkGroupOrderLimitWellTyped tEnv colEnv head types (OrderBy _ cols) = checkGroupOrderLimitWellTyped tEnv colEnv head types (GroupBy cols)
checkGroupOrderLimitWellTyped _ _ _ _ _ = True
                                                                     

--Function that checks whether the product condition is well typed
--      The second argument is the header of the first table in the product
--      The third argument is the header of the second table in the product
--      The fourth argument is the type of the first table in the product
--      The fifth argument is the type of the second table in the product
checkProductConditionWellTyped :: TypeEnvironment -> [String] -> [String] -> [Type] -> [Type] -> ProductCondition -> Bool
checkProductConditionWellTyped tEnv head1 head2 types1 types2 (ProdCondition compOp c1 c2) = case computeTypeComparisonOp evalC1 evalC2 compOp of
                                                                                                  Bool -> True
                                                                                              where
                                                                                                evalC1 = typeOfCol tEnv [] head1 types1 c1
                                                                                                evalC2 = typeOfCol tEnv [] head2 types2 c2
                                                                                                
checkProductConditionWellTyped tEnv head1 head2 types1 types2 (ProdNotCondition prd) = checkProductConditionWellTyped tEnv head1 head2 types1 types2 prd
checkProductConditionWellTyped tEnv head1 head2 types1 types2 (ProdBiCondition _ prd1 prd2) = (checkProductConditionWellTyped tEnv head1 head2 types1 types2 prd1) && (checkProductConditionWellTyped tEnv head1 head2 types1 types2 prd2)
                                                                                       
                                                         
