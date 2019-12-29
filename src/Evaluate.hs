module Evaluate where

import Expr
import Parser
import Statement
import qualified Data.Map as M

data Value = Nil
           | VInt Int
           | VBool Bool
           | VFun [String] [Statement]
    deriving (Show, Eq)

type Environment = M.Map String Value 

evalArith :: [Environment] -> Expr -> IO Int
evalArith _ (IntLit x) = return x
evalArith env (Var name) = case find name env of
        Nothing -> error (show name ++ " not declared.")
        Just (VInt x) -> return x
        Just _ -> error $ show name ++ " isn't integer."
evalArith env (FunCall name args) = do
        returned <-evalFunCall env name args
        case returned of
            VInt x -> return x
            _ -> error $ show name ++ " didn't return an integer."
evalArith env (Unary Neg x) = negate <$> evalArith env x
evalArith env (Binary op l r) = fOp <$> evalArith env l <*> evalArith env r
    where fOp = case op of
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> div
            Mod -> mod
evalArith _ _ = error $ "Invalid term in arithmetic expression"

evalBool :: [Environment] -> Expr -> IO Bool
evalBool env (BoolLit b) = return b
evalBool env (Var name) = case find name env of
    Nothing -> error (show name ++ " not declared.")
    Just (VBool x) -> return x
    Just _ -> error $ show name ++ " isn't boolean."
evalBool env (FunCall name args) = do
    returned <-evalFunCall env name args
    case returned of
        VBool x -> return x
        _ -> error $ show name ++ " didn't return a boolean."
evalBool env (Unary Not x) = not <$> evalBool env x
evalBool env (Binary op l r) = case op of
    And -> (&&) <$> evalBool env l <*> evalBool env r
    Or ->  (||) <$> evalBool env l <*> evalBool env r

    Less ->  (<) <$> evalArith env l <*> evalArith env r
    Greater ->  (>) <$> evalArith env l <*> evalArith env r
    LessOrEq ->  (<=) <$> evalArith env l <*> evalArith env r
    GreatOrEq ->  (>=) <$> evalArith env l <*> evalArith env r
    Equal ->  (==) <$> evalArith env l <*> evalArith env r
    NotEqual ->  (/=) <$> evalArith env l <*> evalArith env r
evalBool _ _ = error $ "Invalid term in boolean expression"

evalExpr :: [Environment] -> Expr -> IO Value
evalExpr env (IntLit x) = return $ VInt x
evalExpr env (BoolLit b) = return $ VBool b
evalExpr env (Var name) = case find name env of
    Nothing -> error (show name ++ " not declared.")
    Just x -> return x
evalExpr env (FunCall name args) = evalFunCall env name args
evalExpr env (Unary op x) = case op of
    Not -> VBool <$> not <$> evalBool env x
    Neg -> VInt <$> negate <$> evalArith env x
evalExpr env (Binary op l r) = case op of
    Add -> fmap VInt $ (+) <$> evalArith env l <*> evalArith env r
    Sub -> fmap VInt $ (-) <$> evalArith env l <*> evalArith env r
    Mul -> fmap VInt $ (*) <$> evalArith env l <*> evalArith env r
    Div -> fmap VInt $ div <$> evalArith env l <*> evalArith env r
    Mod -> fmap VInt $ mod <$> evalArith env l <*> evalArith env r

    And -> fmap VBool $ (&&) <$> evalBool env l <*> evalBool env r
    Or -> fmap VBool $ (||) <$> evalBool env l <*> evalBool env r

    Less -> fmap VBool $ (<) <$> evalArith env l <*> evalArith env r
    Greater -> fmap VBool $ (>) <$> evalArith env l <*> evalArith env r
    LessOrEq -> fmap VBool $ (<=) <$> evalArith env l <*> evalArith env r
    GreatOrEq -> fmap VBool $ (>=) <$> evalArith env l <*> evalArith env r

    Equal -> fmap VBool $ (==) <$> evalExpr env l <*> evalExpr env r
    NotEqual -> fmap VBool $ (/=) <$> evalExpr env l <*> evalExpr env r

evalFunCall :: [Environment] -> String -> [Expr] -> IO Value
evalFunCall env name args = case (find name env) of
    Nothing -> error $ "Function " ++ show name ++ " not declared."
    Just (VFun params body) -> do
        if (length params) == (length args) then do
            let declars = zipWith (\p a -> VarDecl p a) params args
            (returned, newEnv) <- evaluate (emptyEnv:env) (declars++body)
            return returned
        else error $ show name ++ " called with wrong number of arguments."
    Just _ -> error $ show name ++ " isn't a function."

find :: String -> [Environment] -> Maybe Value
find name [] = Nothing
find name (x:xs) = case M.lookup name x of
    Nothing -> find name xs
    Just y -> Just y

modifyFirst ::  (a -> a) -> (a -> Bool) -> [a] -> [a]
modifyFirst _ _ [] = []
modifyFirst f b (x:xs) = if b x then (f x):xs else x:(modifyFirst f b xs) 


emptyEnv :: Environment
emptyEnv = M.empty

evaluate :: [Environment] -> [Statement] -> IO (Value,[Environment])
evaluate env [] = return (Nil, env)
evaluate env (x:xs) = case x of
    Return expr -> do
        val <- evalExpr env expr
        return $ (val, env)

    VarDecl name expr -> case M.lookup name (head env) of
        Nothing -> do
            val <- evalExpr env expr
            evaluate (modifyFirst (M.insert name val) (\_ -> True) env) xs
        Just _ -> error ("Variable already declared: " ++ show name)
    
    FunDecl name params body -> case find name env of
        Nothing -> evaluate (modifyFirst (M.insert name (VFun params body)) (\_ -> True) env) xs
        Just _ -> error ("Function already declared: " ++ show name)

    VarAssign name expr -> case find name env of
        Nothing  -> error ("Variable not declared: " ++ show name)
        Just _ -> do
            val <- evalExpr env expr
            evaluate (modifyFirst (M.insert name val) (M.member name) env) xs
            
    PrintStmt expr -> do
        nr <- evalExpr env expr
        print nr
        evaluate env xs

    IfStmt cond ifBody elseBody ->  do
        b <- evalBool env cond
        if b then do 
            (r,newEnv) <- evaluate (emptyEnv:env) ifBody
            if r /= Nil then return (r,newEnv)
            else evaluate (tail newEnv) xs
        else do
            (r,newEnv) <- evaluate (emptyEnv:env) elseBody
            if r /= Nil then return (r,newEnv)
            else evaluate (tail newEnv) xs
    
    WhileStmt cond body -> do
        b <- evalBool env cond
        if b then do
            (r,newEnv) <- evaluate (emptyEnv:env) body
            if r /= Nil then return (r,newEnv)
            else evaluate (tail newEnv) (x:xs)
        else evaluate env xs

    ForStmt init cond update body -> do
        (r,newEnv) <- evaluate (emptyEnv:env) (init:[WhileStmt cond (body++[update])])
        if r /= Nil then return (r,newEnv)
        else evaluate (tail newEnv) xs
