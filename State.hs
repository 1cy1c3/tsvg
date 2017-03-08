module State (
   Val (..),
   State (..),
   Error,
   St,
   run,
   err,
   set,
   get
   ) where

import qualified Data.Map as M
import AST(Id)
import TinySVG(Point, Graphics, ColorInfo)

{-
  Representation of the global state and the global state transformer
-}
data Val = N Double | P Double Double

instance Show Val where show (N d) = show d

{-
  The global state: mapping variables to values
  and the output messages produced by the program.
-}
data State = State { vars :: M.Map Id Val
                   , output :: [String]
                   , start :: Point
                   , graphics :: [Graphics]
                   , color :: ColorInfo
                   }

{-
  Exceptions (with an error message)
-}
type Error a = Either String a

{-
  The global state transformer: a combination of state transformers
  and exceptions.
-}
data St a = St { run :: State-> Error (a, State) }

{-
  Make St into a Monad.
-}
instance Functor St where
  fmap f p = St $ \s -> fmap (\(a, s) -> (f a, s)) (run p s)

instance Applicative St where
  pure a  = St $ \s -> pure (a, s)
  p <*> q = St $ \s -> do (f, s') <- run p s
                          (a, s'') <- run q s'
                          return (f a, s'')

instance Monad St where
  return = pure
  p >>= q = St $ \s -> do (a, s') <- run p s
                          run (q a) s'

{-
  Elementary operations of the St monad
-}
err :: String -> St a
err msg = St $ \s -> Left msg

set :: (State -> State)-> St ()
set f = St $ \s -> Right ((), f s)

get :: (State -> a) -> St a
get f = St $ \s -> Right (f s, s)
