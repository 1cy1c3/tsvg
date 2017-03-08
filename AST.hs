module AST where

type Id = String

data Expr = Var Id
          | Number Double
          | Negate Expr
          | Plus Expr Expr
          | Times Expr Expr
          | Div Expr Expr
          | LessEq Expr Expr
          | Eq Expr Expr
          | Pte Expr Expr          
          deriving (Eq, Show)

data Cmd = While Expr Cmds
         | If Expr Cmds Cmds
         | Assign Id Expr
         | Print Expr
         | Move Expr
         | Line Expr
         | Curve Expr Expr Expr
         | Circle Expr
         | Slice Expr Expr Expr
         | Arc Expr Expr Expr
         | Colour Id
         deriving (Eq, Show)

type Cmds = [Cmd]

type Decls = [Id]

data Prog = Prog Decls Cmds
          | Tsvg Id (Double, Double) Decls Cmds
          deriving (Eq, Show)
