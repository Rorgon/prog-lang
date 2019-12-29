{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Statement where

import Parser
import Expr
import Data.Char (isDigit, isSpace, isLetter,isAlphaNum)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Control.Applicative hiding (some, many)


data Statement = IfStmt Expr [Statement] [Statement]
               | WhileStmt Expr [Statement]
               | ForStmt Statement Expr Statement [Statement]
               | VarAssign String Expr
               | VarDecl String Expr
               | FunDecl String [String] [Statement]
               | PrintStmt Expr
               | Return Expr
            deriving (Show, Eq)

program :: Parser Text [Statement]
program = token $ many ((varDecl <* char ';') <|> funDecl)

{-
block :: Parser Text Statement
block = do
    b <- token $ bracket (char '{') program (char '}')
    return $ Block b
-}

block :: Parser Text [Statement]
block = bracket (token $ char '{') (many stmt) (token $ char '}')


stmt :: Parser Text Statement
stmt =  forStmt <|>
        ifStmt <|> 
        whileStmt <|>
        printStmt <* char ';' <|> 
        varDecl <* char ';' <|> 
        varAssign <* char ';' <|>
        returnStmt <* char ';'

varDecl :: Parser Text Statement
varDecl = do
    token $ string "var"
    v <- token $ identifier
    e <- (char '=' *> token expr) <|> pure (IntLit 0)
    return $ VarDecl v e

varAssign :: Parser Text Statement
varAssign = do
    v <- token $ identifier
    char '='
    e <- token $ expr
    return $ VarAssign v e

ifStmt :: Parser Text Statement
ifStmt = do
    token $ string "if"
    cond <- token $ bracket (char '(') bExpr (char ')')
    ifBody <- token $ block <|> ((:[]) <$> stmt)
    elseBody <- token $ (string "else" *> (block <|> ((:[]) <$> stmt))) <|> none
    return $ IfStmt cond ifBody elseBody

whileStmt :: Parser Text Statement
whileStmt = do
    token $ string "while"
    cond <- token $ bracket (char '(') bExpr (char ')')
    body <- token $ block <|> ((:[]) <$> stmt)
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
    body <- token $ block <|> ((:[]) <$> stmt)
    return $ ForStmt init cond update body

returnStmt :: Parser Text Statement
returnStmt = do
    token $ string "return"
    e <- expr
    return $ Return e

printStmt :: Parser Text Statement
printStmt = do
    token $ string "print"
    e <- bracket (char '(') expr (char ')')
    return $ PrintStmt e

funDecl :: Parser Text Statement
funDecl = do
    token $ string "fun"
    name <- token $ identifier
    token $ char '('
    params <- (\x xs -> x++xs) <$> oneOrNone (token $ identifier) <*> many (char ',' *> (token $ identifier))
    token $ char ')'
    body <- block
    return $ FunDecl name params body
