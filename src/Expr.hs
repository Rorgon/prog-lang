{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Expr where

import Parser
import Data.Char (isDigit, isSpace, isLetter,isAlphaNum)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Control.Applicative hiding (some, many)

data AExpr =IntLit Int
          | AVar String
          | ABinary ArithOp AExpr AExpr
            deriving (Show)

data ArithOp = Add | Sub | Mul | Div | Mod
    deriving (Show)

data BoolOp = And | Or
    deriving (Show)

data RelOp = Less | Greater | LessOrEq | GreatOrEq | Equal | NotEqual
    deriving (Show)

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BoolOp BExpr BExpr
           | RBinary RelOp AExpr AExpr
            deriving (Show)
           

addop :: Parser Text (AExpr->AExpr->AExpr)
addop = do
    op <- token $ char '+' <|> char '-'
    return $ case op of
        '+' -> ABinary Add
        '-' -> ABinary Sub

mulop :: Parser Text (AExpr->AExpr->AExpr)
mulop = do
    op <- token $ char '*' <|> char '/' <|> char '%' 
    return $ case op of
        '*' -> ABinary Mul
        '/' -> ABinary Div
        '%' -> ABinary Mod

literal :: Parser Text AExpr
literal = do
    nr <- token number
    return (IntLit nr)

aVar :: Parser Text AExpr
aVar = do
    name <- token identifier
    return (AVar name)

aFactor :: Parser Text AExpr
aFactor = do
    fac <- token $ aVar <|> literal <|> bracket (char '(') aExpr (char ')')
    return fac

aExpr :: Parser Text AExpr
aExpr = chainl1 (chainl1 aFactor mulop) addop

andop :: Parser Text (BExpr -> BExpr -> BExpr)
andop = do
    op <- token $ string "and"
    return (BBinary And)

orop :: Parser Text (BExpr -> BExpr -> BExpr)
orop = do
    op <- token $ string "or"
    return (BBinary Or)

boolean :: Parser Text BExpr
boolean = do
    boolean <- token $ string "true" <|> string "false"
    return (BoolConst $ if boolean == "true" then True else False)




rExpr :: Parser Text BExpr
rExpr = do
    fac1 <- aExpr
    op <- choice $ map string ["==","!=",">=","<=",">","<"]
    fac2 <- aExpr
    return $ case op of
        "==" -> RBinary Equal fac1 fac2
        "!=" -> RBinary NotEqual fac1 fac2
        ">=" -> RBinary GreatOrEq fac1 fac2
        "<=" -> RBinary LessOrEq fac1 fac2
        ">"  -> RBinary Greater fac1 fac2
        "<"  -> RBinary Less fac1 fac2

bFactor :: Parser Text BExpr
bFactor = do
    fac <- token $ boolean <|> rExpr <|> bracket (char '(') bExpr (char ')')
    return fac

notBFactor :: Parser Text BExpr
notBFactor = do
    token $ string "not"
    fac <- bFactor
    return (Not fac)

bExpr :: Parser Text BExpr
bExpr = chainl1 (chainl1 (bFactor<|>notBFactor) andop) orop



