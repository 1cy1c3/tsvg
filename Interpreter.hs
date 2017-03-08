module Interpreter where

import Prelude hiding (lookup,update,enter)
import Data.Maybe (fromJust, listToMaybe, fromMaybe)
import Data.Char (isDigit, toLower)
import System.FilePath ((<.>))
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map as M

import AST
import Parser
import State
import TinySVG

{-
  Represent booleans.
-}
isTrue :: Val-> Bool
isTrue (N d) = d /= 0

boolVal :: Bool-> Val
boolVal b = N $ if b then 1 else 0

{-
  Determine the next color regarding an image.
-}
nextColor :: ColorInfo -> ColorInfo
nextColor (NamedColor "black")  = NamedColor "red"
nextColor (NamedColor "red")    = NamedColor "orange"
nextColor (NamedColor "orange") = NamedColor "yellow"
nextColor (NamedColor "yellow") = NamedColor "green"
nextColor (NamedColor "green")  = NamedColor "blue"
nextColor (NamedColor "blue")   = NamedColor "indigo"
nextColor (NamedColor "indigo") = NamedColor "violet"
nextColor (NamedColor "violet") = NamedColor "black"

{-
  Execute a single command.
-}
exec :: Cmd -> St ()
exec w@(While e cs) = do
   v  <- evaluate e
   vs <- get vars
   if isTrue v then do {execs cs; exec w} else return ()
exec (If e cs1 cs2) = do
   v <- evaluate e
   if isTrue v then execs cs1 else execs cs2
exec (Assign i e) = do
   v <- evaluate e
   set $ \s -> s{vars = M.insert i v (vars s)}
exec (Print e) = do
   v<- evaluate e
   set $ \s -> s{output = show v : output s}
exec (Move e) = do
   v <- evaluate e
   case v of
     N v1    -> err "Move: There is no value operation!"
     P p1 p2 -> set $ \s -> s{start = pt (p1, p2) }
exec (Circle e) = do
   v <- evaluate e
   case v of
     P p1 p2 -> err "Circle: There is no point operation!"
     N v1    -> set $ \s -> s{graphics = fillColor (color s) (circle (start s) v1) : 
                (graphics s)}
exec (Colour id) = do
   case id of
     "next" -> set $ \s -> s{color = nextColor (color s)}
     _      -> set $ \s -> s{color = NamedColor id}
exec (Curve e1 e2 e3) = do
   v1 <- evaluate e1
   v2 <- evaluate e2
   v3 <- evaluate e3
   case (v1, v2, v3) of 
     (P p1 p2, P p1' p2', P p1'' p2'') -> set $ \s -> s {graphics = (noFill $ 
        strokeColor (color s) $ path [(moveto (start s)), (cbezier (pt (p1, p2)) 
        (pt (p1', p2')) (pt (p1'', p2'')))]) : (graphics s), start = pt (p1'', p2'')}
exec (Line e) = do
  v <- evaluate e
  case v of 
    N v1    -> err "Move: There is no value operation!"
    P p1 p2 -> set $ \s -> s{graphics = (noFill $ strokeColor (color s) $ 
      path [(moveto (start s)), (lineto (pt (p1, p2)))]) : 
      (graphics s), start = pt (p1, p2)}
exec (Arc e1 e2 e3) = do
   v1 <- evaluate e1
   v2 <- evaluate e2
   v3 <- evaluate e3
   case (v1, v2, v3) of 
     (N n1, N n2, N n3) -> set $ \s -> s{graphics = (strokeColor (color s) $ 
        fillColor (color s) (arc (start s) n1 n2 n3)) : (graphics s)}
exec (Slice e1 e2 e3) = do
   v1 <- evaluate e1
   v2 <- evaluate e2
   v3 <- evaluate e3
   case (v1, v2, v3) of 
     (N n1, N n2, N n3) -> set $ \s -> s{graphics = (strokeColor (color s) $ 
        fillColor (color s) (slice (start s) n1 n2 n3)) : (graphics s)}

{-
  Execute a sequence of commands.
-}
execs :: [Cmd]-> St ()
execs = mapM_ exec

{-
  Evaluate an expression.
-}
evaluate :: Expr -> St Val
evaluate e = do
   ev <- get $ eval e
   case ev of Right v -> return v
              Left  e -> err e

{-
  Evaluate an expression.
  (no return state)
-}
eval :: Expr -> State -> Error Val
eval (Var i) s =
  case M.lookup i (vars s) of
    Just v  -> return v
    Nothing -> Left $ "Undeclared variable: " ++ i
eval (Number d) _ = return $ N d
eval (Pte p1 p2) s = do
  v1 <- eval p1 s
  v2 <- eval p2 s
  case (v1, v2) of 
    (N d1, N d2) -> return $ P d1 d2
eval (Negate e) s = do
  v <- eval e s
  case v of
    N d -> return $ N (-d)
eval (Plus e1 e2) s = do
  v1 <- eval e1 s
  v2 <- eval e2 s
  case (v1, v2) of
    (N d1, N d2)         -> return $ N (d1 + d2)
    (P d1 d2, P d1' d2') -> return $ P (d1 + d2) (d1' + d2')
eval (Times e1 e2) s = do
  v1 <- eval e1 s
  v2 <- eval e2 s
  case (v1, v2) of
    (N d1, N d2) -> return $ N $ d1 * d2
    (P d1 d2, N v1) -> return $ P (v1 * d1) (v1 * d2)
    (N v1, P d1 d2) -> return $ P (v1 * d1) (v1 * d2)
eval (Div e1 e2) s = do
  v1 <- eval e1 s
  v2 <- eval e2 s
  case (v1, v2) of
    (N d1, N d2) | d2 == 0 -> Left "Division by zero."
    (N d1, N d2)           -> return $ N $ d1 / d2
eval (LessEq e1 e2) s = do
  v1 <- eval e1 s
  v2 <- eval e2 s
  case (v1, v2) of
    (N d1, N d2) -> return $ boolVal (d1 <= d2)
eval (Eq e1 e2) s = do
  v1 <- eval e1 s
  v2 <- eval e2 s
  case (v1, v2) of
    (N d1, N d2)         -> return $ boolVal (d1 == d2)
    (P d1 d2, P d1' d2') -> return $ boolVal $ (d1 == d1') && (d2 == d2')

{-
  Evaluate a sequence of declarations.
-}
decls :: [Id]-> St ()
decls = mapM_ (\i -> set $ \s -> s{vars = M.insert i (N 0) (vars s)})

{-
  Evaluate a whole program.
-}
prog :: Prog-> IO ()
prog (Prog ds cs) = do
  let i = initialState 
  case run (do {decls ds; execs cs}) i of
     Left err      -> putStrLn $ "*** ERROR: " ++ err
     Right ((), s) -> do
        mapM_ putStrLn (reverse (output s))

prog (Tsvg id (w, h) ds cs) = do
  let i = initialState { start = pt ((w/2), (h/2)) }
  case run (do {decls ds; execs cs}) i of
     Left err      -> putStrLn $ "*** ERROR: " ++ err
     Right ((), s) -> do
        writeFile (id ++ ".svg") (toXML w h (group (graphics s)))

initialState :: State
initialState =
  State { vars = M.empty
        , output = []
        , start = pt (500, 500)
        , graphics = []
        , color = NamedColor "black"
        }
