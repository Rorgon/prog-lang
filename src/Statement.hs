{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Statement where

import Parser
import Expr
import Data.Char (isDigit, isSpace, isLetter,isAlphaNum)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Control.Applicative hiding (some, many)


data Statement = IfStmt BExpr [Statement]
               | WhileStmt BExpr [Statement]
               | ForStmt Statement BExpr Statement [Statement]
               | VarAssign String AExpr
               | VarDecl String AExpr
               | PrintStmt AExpr

            deriving (Show)

program :: Parser Text [Statement]
program = token $ many stmt

{- TODO
block :: Parser Text Statement
block = do
    b <- token $ bracket (char '{') program (char '}')
    return $ Block b -}

block :: Parser Text [Statement]
block = bracket (char '{') program (char '}')

stmt :: Parser Text Statement
stmt =  forStmt <|>
        ifStmt <|> 
        whileStmt <|>
        printStmt <* char ';' <|> 
        varDecl <* char ';' <|> 
        varAssign <* char ';'

varDecl :: Parser Text Statement
varDecl = do
    token $ string "var"
    v <- token $ identifier
    e <- (char '=' *> token aExpr)<|> pure (IntLit 0)
    return $ VarDecl v e

varAssign :: Parser Text Statement
varAssign = do
    v <- token $ identifier
    char '='
    e <- token $ aExpr
    return $ VarAssign v e

ifStmt :: Parser Text Statement
ifStmt = do
    token $ string "if"
    cond <- token $ bracket (char '(') bExpr (char ')')
    body <- token $ block
    return $ IfStmt cond body

whileStmt :: Parser Text Statement
whileStmt = do
    token $ string "while"
    cond <- token $ bracket (char '(') bExpr (char ')')
    body <- token $ block
    return $ WhileStmt cond body

forStmt :: Parser Text Statement
forStmt = do
    token $ string "for"
    char '('
    init <- token $ varDecl <|> varAssign
    char ';'
    cond <- token $ bExpr
    char ';'
    update <- token $ varAssign
    char ')'
    body <- token $ block
    return $ ForStmt init cond update body



printStmt :: Parser Text Statement
printStmt = do
    token $ string "print"
    e <- bracket (char '(') aExpr (char ')')
    return $ PrintStmt e
