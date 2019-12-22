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

main :: IO ()
main = do
    args <- getArgs
    file <- readFile (head args)
    let ast = runParser program $ T.pack file
    case ast of
        Right (tree, rest) -> if T.length rest == 0 
            then do 
                evaluate [emptyEnv] tree
                return ()
            else print "Syntax Error"
        Left e -> print e
