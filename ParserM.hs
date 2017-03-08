module ParserM (
   Parser, parse
 , satisfy, token, tokens, (+++)
 , choice, opt, many, many1
 , sepby, sepby1, brackets
 ) where

import Control.Monad
import Control.Applicative hiding (many)
import Data.Char(isDigit,isSpace,isLetter)

{-
  Monadic combinator parsing
  Taken from: Hutton, Meier: Monadic Parsing in Haskel
  http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
-}

data Parser alpha beta = Parser { parse :: [alpha] -> [(beta, [alpha])] }

instance Functor (Parser a) where
    fmap f (Parser p) = Parser $ \cs -> do 
        (a, as) <- p cs
        return (f a, as)  

instance Applicative (Parser a) where
    pure a = Parser $ \s -> [(a, s)]
    (Parser f) <*> (Parser p) = Parser $ \cs -> do
        (f', cs') <- f cs
        (a, cs'') <- p cs' 
        return (f' a, cs'')
 
instance Monad (Parser a) where
    return = pure
    (Parser p) >>= f = Parser $ \cs -> do
        (a, cs') <- p cs
        parse (f a) cs'

instance Alternative (Parser a) where
    empty = Parser $ const []
    (Parser p) <|> (Parser q) = Parser $ \cs -> case p cs of
        [] -> q cs 
        s  -> s

instance MonadPlus (Parser a)  where
    mzero = empty
    mplus a b = Parser $ \cs -> parse a cs ++ parse b cs 

{-
  Recognize tokens.
-}
satisfy :: (alpha-> Bool)-> Parser alpha alpha
satisfy p = Parser $ \s-> case s of
       []     -> []
       (x:xs) -> if p x then [(x, xs)] else []

token :: Eq a => a -> Parser a a
token c = satisfy (c ==)

tokens :: Eq a => [a] -> Parser a [a]
tokens [] = pure []
tokens (x:xs) = do
    token x
    tokens xs
    return (x:xs)

{-
  Aging with Backtracking.
-}
(+++):: Parser a b -> Parser a b -> Parser a b
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                            []      -> []
                            (x:xs)  -> [x])

{-
  Choice from lists.
-}
choice :: [Parser a b] -> Parser a b
choice (p:[]) = p 
choice (p:ps) = p +++ choice ps

opt :: Parser a b -> Parser a (Maybe b)
opt p = (fmap Just  p) +++ return Nothing

{-
  Kleene star (as often as possible)
-}
many :: Parser a b -> Parser a [b]
many p = many1 p +++ return []

{-
  Kleene plus (at least once)
-}
many1 :: Parser a b -> Parser a [b]
many1 p = do 
    a <- p
    as <- many p
    return (a:as)

{-
  As often as possible, separated through one token.
-}
sepby :: Eq a=> Parser a b -> a -> Parser a [b]
p `sepby` sep = (p `sepby1` sep) +++ return []

{-
  At least once, separated through one token.
-}
sepby1 :: Eq a=> Parser a b -> a -> Parser a [b]
p `sepby1` sep = do 
    a <- p
    as <- many (do {token sep; p})
    return (a:as)

{-
  Left to right (braided)
-}
brackets :: Eq a=> a-> Parser a b-> a-> Parser a b
brackets left p right = do { token left; r<- p; token right; return r }
