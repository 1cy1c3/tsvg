module Parser where

import Data.Char
import Data.Maybe
import Control.Exception

import ParserM hiding (Parser)
import qualified ParserM as P
import AST

import State (Error)
type Parser = P.Parser Char

{-
  Variable ::= Char (Char|Digit)^*
-}
identifier :: Parser String
identifier = do 
  c <- satisfy isLetter;
  cs <- many $ satisfy isAlphaNum
  return $ c:cs

{-
  Number ::= Digit+ ('.' Digit+)?
-}
digitseq = many1 $ satisfy isDigit
number :: Parser Double
number = do
  ds <- digitseq;
  ds2 <- opt (do {t <- token '.'; ds <- digitseq; return $ t:ds })
  return $ read $ ds ++ fromMaybe "" ds2

{-
  Factor ::= Identifier | Number | '(' Expr ')' | '-' Expr
-}
factor :: Parser Expr
factor = fmap Var identifier +++ fmap Number number 
         +++ do { token '-'; e <- expr; return $ Negate e }
         +++ brackets '(' expr ')'

{-
  Term ::= Factor '*' Term  | Factor '/' Term | Factor 
-}
term :: Parser Expr
term = 
  do { e1 <- factor; token '*'; e2 <- term; return $ Times e1 e2 }
  +++
  do { e1 <- factor; token '/'; e2 <- term; return $ Div e1 e2 }
  +++ factor 

{-
  ATerm ::= Term '+' ATerm | Term
-}
aterm :: Parser Expr
aterm = do { e1 <- term; token '+'; e2 <- aterm; return $ Plus e1 e2 }
  +++ term 

{-
  Expr ::= ATerm '<=' ATerm | ATerm '=' ATerm | ATerm
-}
expr :: Parser Expr
expr =
  do { e1 <- aterm; tokens "<="; e2 <- aterm; return $ LessEq e1 e2 }
  +++
  do { e1 <- aterm; token '='; e2 <- aterm; return $ Eq e1 e2 }
  +++ aterm 

{-
  cmd ::= Possible commands
-}
cmd :: Parser Cmd
cmd = choice [printStmt, whileStmt, ifStmt, assignStmt, curveStmt, 
  lineStmt, moveStmt, circleStmt, arcStmt, sliceStmt, colourStmt]
  
{-
  Expression 'print'
-}
printStmt :: Parser Cmd
printStmt = do { tokens "print"; e <- expr; return $ Print e }

{-
  Expression 'while' (Sequence)
-}
whileStmt :: Parser Cmd
whileStmt = do
  tokens "while"; e <- expr; cs <- brCmdSeq;
  return $ While e cs

{-
  Expression 'if' (Sequence)
-}
ifStmt :: Parser Cmd
ifStmt = do
  tokens "if"; e <- expr; cs <- brCmdSeq
  ds <- opt (do { tokens "else"; brCmdSeq })
  return $ If e cs (fromMaybe [] ds) 

{-
  Parser regarding expressions like '<e1,e2>'
-}
pointParser :: Parser Expr
pointParser = do
  tokens "<"; e <- expr; tokens ","; e2 <- expr; tokens ">";
  return $ Pte e e2

{-
  Expression 'curve'
-}
curveStmt :: Parser Cmd
curveStmt = do
  tokens "curve"; tokens "("; e <- pointParser; tokens ","; 
  e2 <- pointParser; tokens ","; e3 <- pointParser; tokens ")" 
  return $ Curve e e2 e3
  
{-
  Expression 'line'
-}
lineStmt :: Parser Cmd
lineStmt = do
  tokens "line"; e <- pointParser;
  return $ Line e 

{-
  Expression 'move'
-}
moveStmt :: Parser Cmd
moveStmt = do
  tokens "move"; e <- pointParser;
  return $ Move e 
  
{-
  Expression 'circle'
-}
circleStmt :: Parser Cmd
circleStmt = do
  tokens "circle"; e <- expr;
  return $ Circle e 
  
{-
  Expression 'arc'
-}
arcStmt :: Parser Cmd 
arcStmt = do
  tokens "arc"; tokens "("; e <- expr; tokens ","; e2 <- expr; 
  tokens ","; e3 <- expr; tokens ")" 
  return $ Arc e e2 e3
  
{-
  Expression 'slice'
-}
sliceStmt :: Parser Cmd 
sliceStmt = do
  tokens "slice"; tokens "("; e <- expr; tokens ","; e2 <- expr; 
  tokens ","; e3 <- expr; tokens ")" 
  return $ Slice e e2 e3
  
{-
  Expression 'colour'
-}
colourStmt :: Parser Cmd
colourStmt = do
  tokens "colour"; e <- identifier;
  return $ Colour e 

{-
  Expression 'assign' (:=)
-}
assignStmt :: Parser Cmd
assignStmt =
  do { id <- identifier; tokens ":="; e <- expr; return $ Assign id e }

{-
  brcmdseq ::= '{' cmds '}'
  cmds     ::= cmd ';' cmds | cmd
-}
brCmdSeq :: Parser [Cmd]
brCmdSeq = brackets '{' (sepby1 cmd ';') '}'

{-
  Parser regarding IMP
-}
normalProgram :: Parser Prog
normalProgram = do
  decls <- many (do { tokens "var"; id <- identifier; token ';'; return id})
  cmds <- sepby cmd ';'
  return $ Prog decls cmds

{-
  Parser regarding TSVG
-}
tsvgProgram :: Parser Prog
tsvgProgram = do 
  tokens "tsvg"
  id <- identifier
  tokens "("; i1 <- number; tokens ","; i2 <- number; tokens ");"
  decls <- many (do { tokens "var"; id <- identifier; token ';'; return id})
  cmds <- sepby cmd ';'
  return $ Tsvg id (i1, i2) decls cmds
  
{-
  Parser 2 regarding TSVG
  (without a size)
-}
tsvgProgram2 :: Parser Prog
tsvgProgram2 = do 
  tokens "tsvg"
  id <- identifier 
  tokens ";"
  decls <- many (do { tokens "var"; id <- identifier; token ';'; return id})
  cmds <- sepby cmd ';'
  return $ Tsvg id (1000, 1000) decls cmds

{-
  Possible parser
-}
program :: Parser Prog
program = choice [tsvgProgram2, tsvgProgram, normalProgram]

{-
  Start parsing.
-}
parseMain :: String -> Error Prog
parseMain inp = case parse program (filter (not . isSpace) inp) of
  [(st, [])] -> Right st
  [(st, r)] -> Left $ "Parse error on input '" ++ [head r] 
    ++ "' (" ++ show (length inp - length r) ++ ")" ++ " Rest: " 
    ++ tail r
  _ -> Left $ "Parse error (no parse)"

{-
  Parse a string from file.
-}
parseFromFile :: String -> IO (Error Prog)
parseFromFile filenm = 
  catch (do cont <- readFile filenm
            return $ parseMain cont)
        (\e-> return $ Left $ "I/O error: " ++ show (e :: IOError))
