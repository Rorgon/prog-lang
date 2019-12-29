{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Expr where

import Parser
import Data.Char (isDigit, isSpace, isLetter,isAlphaNum)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Control.Applicative hiding (some, many)


data Expr  = IntLit Int
           | BoolLit Bool
    --       | StringLit String
           | Var String
           | FunCall String [Expr]
           | Binary BinaryOp Expr Expr
           | Unary UnaryOp Expr
        deriving (Show, Eq)

data UnaryOp = Neg | Not
        deriving (Show, Eq)

data BinaryOp = Add | Sub | Mul | Div | Mod
              | And | Or
        --      | Concat
              | Less | Greater | LessOrEq | GreatOrEq | Equal | NotEqual
        deriving (Show, Eq)


addop :: Parser Text (Expr->Expr->Expr)
addop = do
    op <- token $ char '+' <|> char '-'
    return $ case op of
        '+' -> Binary Add
        '-' -> Binary Sub

mulop :: Parser Text (Expr->Expr->Expr)
mulop = do
    op <- token $ char '*' <|> char '/' <|> char '%' 
    return $ case op of
        '*' -> Binary Mul
        '/' -> Binary Div
        '%' -> Binary Mod

literal :: Parser Text Expr
literal = do
    nr <- token number
    return (IntLit nr)

var :: Parser Text Expr
var = do
    name <- token identifier
    return (Var name)

aFactor :: Parser Text Expr
aFactor = do
    fac <- token $ fCall <|> var <|> literal <|> bracket (char '(') aExpr (char ')')
    return fac

negAFactor :: Parser Text Expr
negAFactor = do
    fac <- token $ char '-' *> aFactor
    return (Unary Neg fac)

aExpr :: Parser Text Expr
aExpr = chainl1 (chainl1 (aFactor <|> negAFactor ) mulop) addop

andop :: Parser Text (Expr -> Expr -> Expr)
andop = do
    op <- token $ string "and"
    return (Binary And)

orop :: Parser Text (Expr -> Expr -> Expr)
orop = do
    op <- token $ string "or"
    return (Binary Or)

boolean :: Parser Text Expr
boolean = do
    boolean <- token $ string "true" <|> string "false"
    return (BoolLit $ if boolean == "true" then True else False)


rExpr :: Parser Text Expr
rExpr = do
    fac1 <- aExpr
    op <- choice $ map string ["==","!=",">=","<=",">","<"]
    fac2 <- aExpr
    return $ case op of
        "==" -> Binary Equal fac1 fac2
        "!=" -> Binary NotEqual fac1 fac2
        ">=" -> Binary GreatOrEq fac1 fac2
        "<=" -> Binary LessOrEq fac1 fac2
        ">"  -> Binary Greater fac1 fac2
        "<"  -> Binary Less fac1 fac2

bFactor :: Parser Text Expr
bFactor = do
    fac <- token $ boolean <|> rExpr <|> fCall <|> var <|> bracket (char '(') bExpr (char ')')
    return fac

notBFactor :: Parser Text Expr
notBFactor = do
    token $ string "not"
    fac <- bFactor
    return (Unary Not fac)

bExpr :: Parser Text Expr
bExpr = chainl1 (chainl1 (notBFactor <|> bFactor) andop) orop

{-nil :: Parser Text Expr
nil = do
    string "nil"
    return (Nil)
-}

expr :: Parser Text Expr
expr = chainl1 (chainl1 (notBFactor <|> bFactor <|> negAFactor <|> aFactor) (andop <|> mulop)) (orop <|> addop)


fCall :: Parser Text Expr
fCall = do
    name <- token $ identifier
    token $ char '('
    exprs <- (\x xs -> x++xs) <$> oneOrNone expr <*> many (char ',' *> expr)
    token $ char ')'
    return $ FunCall name exprs


