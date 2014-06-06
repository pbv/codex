{-# LANGUAGE OverloadedStrings #-}
-- A DSL for queries to the submission database
module Query where

import           Data.ByteString.UTF8(ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.String

{-
import           System.Directory
import           System.Directory.Tree

import qualified Data.List as List
import Data.Time
import Problem
import Submission
-}

-- values
data Value = IntVal !Int
           | DoubleVal !Double
           | BoolVal !Bool
           | StringVal !String
--         | TimeVal !UTCTime
             deriving (Show, Eq, Ord)

instance IsString Value where
  fromString = StringVal 


instance Num Value where
  fromInteger n = IntVal (fromIntegral n)
  -- addition
  IntVal x + IntVal y       = IntVal (x+y)
  DoubleVal x + IntVal y    = DoubleVal (x+fromIntegral y)  
  IntVal x + DoubleVal y    = DoubleVal (fromIntegral x+y)  
  DoubleVal x + DoubleVal y = DoubleVal (x+y)
  -- multiplication
  IntVal x * IntVal y       = IntVal (x*y)
  --
  abs (IntVal x)            = IntVal (abs x)
  signum (IntVal x)         = IntVal (signum x)
  

-- expressions
data Expr = Lit Value
          | App String [Expr]
          | Var String
          deriving Show


(.==.), (.<=.) :: Expr -> Expr -> Expr
e1 .==. e2 = App "==" [e1, e2]
e1 .<=. e2 = App "<=" [e1, e2]


(.+.) :: Expr -> Expr -> Expr
e1 .+. e2 = App "*" [e1, e2]

-- queries
data Query = Restrict Expr Query
           | Select [(String,Expr)] Query
           | SummarizeBy [String] [(String,Expr)] Query
           | RelVar String
             deriving Show


-- tuples
type Tuple = [Value]

-- relations
data Relation = Rel [String] [Tuple]
                 deriving Show



type ExprEnv = [(String,Value)]

evalExpr :: ExprEnv -> Expr -> Value
evalExpr env (Lit v) = v
evalExpr env (Var x) = 
  case lookup x env of 
    Just v -> v
    Nothing -> error ("unknown expr variable: "++show x)
evalExpr env (App op exprs) 
  = evalOp op (map (evalExpr env) exprs)


evalOp "==" [v1, v2] = BoolVal (v1==v2)
evalOp "/=" [v1, v2] = BoolVal (v1/=v2)
evalOp "<=" [v1, v2] = BoolVal (v1<=v2)
evalOp ">=" [v1, v2] = BoolVal (v1>=v2)
evalOp "+" [v1,  v2] = v1+v2
evalOp "*" [v1, v2]  = v1*v2
evalOp op _ = error ("invalid op: "++show op)



type QueryEnv = [(String,Relation)]

evalQuery :: ExprEnv -> QueryEnv -> Query -> Relation
evalQuery env qenv (RelVar x) 
  = case lookup x qenv of
  Just r -> r
  Nothing -> error ("unknown relvar: "++show x)
evalQuery env qenv (Restrict e q) 
  = restrict env e (evalQuery env qenv q)
evalQuery env qenv (Select binds q) 
  = select env binds (evalQuery env qenv q)

restrict :: ExprEnv -> Expr -> Relation -> Relation
restrict env e (Rel cols tuples) = Rel cols tuples'
  where tuples' = [tuple | tuple<-tuples, 
                   let env' = zip cols tuple ++ env,
                   evalExpr env' e == BoolVal True ]


select :: ExprEnv -> [(String,Expr)] -> Relation -> Relation
select env binds (Rel cols tuples) = Rel cols' tuples'
  where cols' = map fst binds
        exprs = map snd binds
        tuples' = [ map (evalExpr env') exprs  | tuple<-tuples,
                     let env' = zip cols tuple ++ env]
                     
        


-- example
phoneBook :: Relation
phoneBook = Rel ["name", "number"] [
  [StringVal "pedro", IntVal 12345],
  [StringVal "pedro", IntVal 67898],
  [StringVal "joão", IntVal 56789]
  ]
            
example1 = Restrict (App "==" [Var "name", Lit (StringVal "Pedro")]) (RelVar "phoneBook")
                                     
example2 = Select [("name", Var "name"),
                   ("number1", App "+" [Var "number", Lit (IntVal 1)])] (RelVar "phoneBook")



{-
-- * submission data rows
type Row = (UID, Problem UTCTime, Submission)

rowStatus :: Row -> Maybe Status
rowStatus (_,_,s) = fmap reportStatus (submitReport s)

accepted :: Row -> Bool
accepted r = rowStatus r == Just Accepted

accepted' :: Row -> Bool
accepted' r = rowStatus r == Just Accepted || rowStatus r == Just Overdue

rejected :: Row -> Bool
rejected = not . accepted


-- count the number of accepted submissions
ex1 :: Query Row Int
ex1 = project rowStatus $ do { select (== Just Accepted); count}

-- count the number of students with accepted submissions
ex2 :: Query Row Int         
ex2 = do select accepted 
         project (\(u,p,s) -> u) $ do {group ; count}
-}

-- query language interpreter
-- top-level queries should be over the Row type
-- the result an 'a' plus a lazy list of rows
{-
runQuery :: Query Row a -> IO (a, [Row])
runQuery q = do top <- readDirectoryWithL readf "submissions"
                return (runQuery' q $ mkRows $ dirTree top)
  where readf file 
          | ext == ".out" = readFromHTMLFile file reportReader
          | otherwise = ioError $ userError "ignored file"
          where ext = takeExtension file
                
-}
