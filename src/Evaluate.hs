module Evaluate where

import Expr
import Parser
import Statement
import qualified Data.Map as M

type Environment = M.Map String Int 

evalArith :: [Environment] -> AExpr -> Either ErrorMsg Int
evalArith _ (IntLit x) = (Right x)
evalArith env (AVar name) = case find name env of
        Nothing -> (Left (show name ++ " not declared."))
        Just x -> Right x
evalArith env (ABinary op l r) = fOp <$> evalArith env l <*> evalArith env r
    where fOp = case op of
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> div
            Mod -> mod

evalBool :: [Environment] -> BExpr -> Either ErrorMsg Bool
evalBool env (BoolConst b) = (Right b)
evalBool env (BBinary op l r) = bOp <$> evalBool env l <*> evalBool env r
    where bOp = case op of
            And -> (&&)
            Or -> (||)
evalBool env (RBinary op l r) = rOp <$> evalArith env l <*> evalArith env r
    where rOp = case op of
            Less -> (<)
            Greater -> (>)
            LessOrEq -> (<=)
            GreatOrEq -> (>=)
            Equal -> (==)
            NotEqual -> (/=)

find :: String -> [Environment] -> Maybe Int
find name [] = Nothing
find name (x:xs) = case M.lookup name x of
    Nothing -> find name xs
    Just y -> Just y

modifyFirst ::  (a -> a) -> (a -> Bool) -> [a] -> [a]
modifyFirst _ _ [] = []
modifyFirst f b (x:xs) = if b x then (f x):xs else x:(modifyFirst f b xs) 

emptyEnv :: Environment
emptyEnv = M.empty

evaluate :: [Environment] -> [Statement] -> IO [Environment]
evaluate env [] = return (tail env)
evaluate env (x:xs) = case x of
    VarDecl name expr -> case find name env of
        Nothing -> case evalArith env expr of
                        Left err -> print err >> return env
                        Right val -> evaluate (modifyFirst (M.insert name val) (\_ -> True) env) xs
        Just _ -> print ("Variable declared twice: " ++ show name) >> return env

    VarAssign name expr -> case find name env of
        Nothing  -> print ("Variable not declared: " ++ show name) >> return env
        Just _ -> case evalArith env expr of
                        Left err -> print err >> return env
                        Right val -> evaluate (modifyFirst (M.insert name val) (M.member name) env) xs
            
    PrintStmt e -> case evalArith env e of
        Left err -> print err >> return env
        Right nr -> do
            print nr
            evaluate env xs

    IfStmt b ifs ->  case evalBool env b of
        Left err -> print err >> return env
        Right True -> do 
            newEnv <- evaluate (emptyEnv:env) ifs
            evaluate newEnv xs
        Right False -> evaluate env xs
    
    WhileStmt b body -> case evalBool env b of
        Left err -> print err >> return env
        Right True -> do
            newEnv <- evaluate (emptyEnv:env) body
            evaluate newEnv (x:xs)
        Right False -> evaluate env xs
    
    ForStmt init cond update body -> do
        newEnv <- evaluate (emptyEnv:env) (init:[WhileStmt cond (body++[update])])
        evaluate newEnv xs