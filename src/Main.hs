{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Parser
import Expr
import Statement
import Evaluate
import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Map as M

findLine :: Int -> Int -> [String] -> Int
findLine counter remaining [] = counter
findLine counter remaining (x:xs)
    | remaining <= 0 = counter
    | otherwise = findLine (counter+1) (remaining-length x) xs

main :: IO ()
main = do
    args <- getArgs
    file <- readFile (head args)
    let ast = runParser program $ T.pack file
    case ast of
        Right (tree, rest) -> if T.length rest == 0 
            then do 
                (_,global) <- evaluate [emptyEnv] tree
                evaluate (emptyEnv:global) [Return $ FunCall "main" []]
                return ()
            else print $ "Syntax Error on line " ++ show (findLine 0 (length file - T.length rest) (lines file))
        Left e -> print e
