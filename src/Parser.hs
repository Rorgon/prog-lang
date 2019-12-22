{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative hiding (many,some)
import Data.Char (isDigit, isSpace, isLetter,isAlphaNum)
import qualified Data.Text as T
import Data.List (isPrefixOf, foldl')
import GHC.Exts (IsList,Item,toList,fromList)

type ErrorMsg = String
--data ParseError = ParseError ErrorMsg Int

type Text = T.Text

data Parser s a = Parser { runParser :: s -> Either ErrorMsg (a,s) }

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x,y)

instance Functor (Parser a) where
    -- fmap :: (a -> b) -> Parser s c -> Parser s b
    fmap f p = Parser $ fmap (first f) . runParser p

instance Applicative (Parser a) where
    -- pure :: b -> Parser a b
    pure x = Parser (\inp -> Right (x,inp))

    --(<*>) :: Parser a (b->c) -> Parser a b -> Parser a c
    p1 <*> p2 = Parser (\inp -> case runParser p1 inp of
                                (Left e) -> Left e
                                (Right (f,inp')) -> case runParser p2 inp' of
                                                    (Left e) -> Left e
                                                    (Right (r,inp'')) -> Right (f r, inp''))

instance Alternative (Parser a) where
    -- empty :: Parser a b
    empty = Parser (\_ -> Left "Alternative empty")

    --(<|>) :: Parser a b -> Parser a b -> Parser a b
    p1 <|> p2 = Parser $ \inp -> case runParser p1 inp of
                                (Left e) -> runParser p2 inp
                                (Right _) -> runParser p1 inp

instance Monad (Parser a) where
    -- return :: b -> Parser a b
    return = pure

    -- (>>=) :: Parser a b -> ( b -> Parser a c) -> Parser a c
    p >>= f = Parser $ \inp -> case runParser p inp of
                                (Left e) -> Left e
                                (Right (r,inp')) -> runParser (f r) inp'

satisfy :: (Char -> Bool) -> Parser Text Char
satisfy p = Parser f
    where f xs
            | T.null xs = Left "Unexpected EOF"
            | p (T.head xs) = Right (T.head xs, T.tail xs)
            | otherwise = Left $ "Unexpected token: " ++ show  (T.length xs)

string :: Text -> Parser Text Text
string s = Parser $ \inp -> if s `T.isPrefixOf` inp then Right (s, T.drop (T.length s) inp) 
                                                  else Left "Syntax Error"          
some :: Parser s a -> Parser s [a]
some p = (:) <$> p <*> (some p <|> pure [])

many :: Parser s a -> Parser s [a]
many p = some p <|> pure []

char :: Char -> Parser Text Char
char c = satisfy (== c)

digit :: Parser Text Char
digit = satisfy isDigit

natural :: Parser Text Int
natural = read <$> some digit

spaces :: Parser Text Text
spaces = T.pack <$> many (satisfy isSpace)

letter :: Parser Text Char
letter = satisfy isLetter

alphaNum :: Parser Text Char
alphaNum = satisfy isAlphaNum

nothing :: Parser s Text
nothing = return T.empty


number :: Parser Text Int
number = do
    sign <- (string "-" <|> nothing)
    nr <- natural
    return ((if T.null sign then 1 else -1) *  nr)

join :: Parser a [b] -> Parser a [b] -> Parser a [b]
p1 `join` p2 = (<>) <$> p1 <*> p2

word :: Parser Text Text
word = T.pack <$> some letter

identifier :: Parser Text String
identifier = some letter `join` many alphaNum

choice :: [Parser a b] -> Parser a b
choice [] = empty
choice (x:xs) = x <|> choice xs

chainl1 :: Parser a b -> Parser a (b->b->b) -> Parser a b
chainl1 p op = do
    x <- p
    fys <- many $ do
        f <- op
        y <- p
        return (f,y)
    return (foldl' (\x (f,y) -> f x y) x fys) 

bracket :: Parser a b -> Parser a c -> Parser a d -> Parser a c
bracket open p close = do
    open
    x <- p
    close
    return x
{-
comment :: Parser String String
comment = do
    string "//"
    comm <- many $ many alphaNum <|> spaces
    string "//"
    return $ concat comm
-}
junk :: Parser Text Text
junk = spaces 

token :: Parser Text a -> Parser Text a
token p = do
    junk
    x <- p
    junk
    return x

    