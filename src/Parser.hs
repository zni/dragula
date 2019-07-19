module Parser where

import Control.Applicative ((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Combinator
import Text.Parsec.Prim (many)

import qualified AST

def = emptyDef{ commentStart = "{"
              , commentEnd   = "}"
              , identStart   = letter
              , identLetter  = alphaNum
              , opStart      = opLetter def
              , opLetter     = oneOf "+-*/="
              , reservedOpNames = [":=", "+", "-", "*", "/",
                                   "=", "<=", "<",
                                   ">=", ">", "#" ]
              , reservedNames = [ "begin", "call", "const", "do",
                                  "end", "if", "odd", "procedure", "then",
                                  "var", "while", "!" ]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , integer = m_integer
           , symbol = m_symbol
           , stringLiteral = m_stringLiteral
           , colon = m_colon
           , dot = m_dot
           , semi = m_semi
           , semiSep = m_semiSep
           , semiSep1 = m_semiSep1
           , comma = m_comma
           , commaSep = m_commaSep
           , commaSep1 = m_commaSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

expr = buildExpressionParser exprTable term

term = m_parens expr
    <|> (m_integer >>= return . AST.IntConst)
    <|> (m_identifier >>= return . AST.Ident)

exprTable = [[binary "*" AST.Mult AssocLeft, binary "/" AST.Div AssocLeft],
             [binary "+" AST.Add AssocLeft, binary "-" AST.Sub AssocLeft],
             [binary "=" AST.Eq AssocLeft, binary "#" AST.NE AssocLeft,
              binary "<" AST.LT AssocLeft, binary "<=" AST.LTE AssocLeft,
              binary ">" AST.GT AssocLeft, binary ">=" AST.GTE AssocLeft]]

binary name fun assoc = Infix (do { m_reservedOp name; return fun }) assoc
prefix name fun = Prefix (do { m_reservedOp name; return fun })

program :: Parser AST.PL0
program = do source <- block
             m_dot
             return $ AST.Program source

block :: Parser AST.PL0
block =  do
    c <- optionMaybe constDecs
    v <- optionMaybe varDecs
    p <- many procedure
    s <- statement
    return $ AST.Block c v p s

varDecs :: Parser AST.PL0
varDecs = do
    try $ m_reserved "var"
    seq <- m_commaSep varDec
    m_semi
    return $ AST.VarDec seq

varDec :: Parser AST.PL0
varDec = do t <- m_identifier
            return $ AST.Var t

constDecs :: Parser AST.PL0
constDecs = do
    try $ m_reserved "const"
    seq <- m_commaSep constDec
    m_semi
    return $ AST.ConstDec seq

constDec :: Parser AST.PL0
constDec = do
    i <- m_identifier
    m_reservedOp "="
    n <- m_integer
    return $ AST.Const i n

procedure :: Parser AST.PL0
procedure = do
    m_reserved "procedure"
    n <- m_identifier
    m_semi
    b <- block
    m_semi
    return $ AST.Procedure n b

statement :: Parser AST.PL0
statement =  assign
         <|> call
         <|> writeln
         <|> begin
         <|> if'
         <|> while

assign :: Parser AST.PL0
assign = do
    n <- m_identifier
    m_reservedOp ":="
    e <- expr
    return $ AST.Assign n e

call :: Parser AST.PL0
call = do
    m_reserved "call"
    n <- m_identifier
    return $ AST.Call n

writeln :: Parser AST.PL0
writeln = do
    m_reserved "!"
    e <- expr
    return $ AST.WriteLn e

begin :: Parser AST.PL0
begin = do
    m_reserved "begin"
    stmts <- m_semiSep statement
    m_reserved "end"
    return $ AST.Begin stmts

if' :: Parser AST.PL0
if' = do
    m_reserved "if"
    e <- expr
    m_reserved "then"
    s <- statement
    return $ AST.If e s

while :: Parser AST.PL0
while = do
    m_reserved "while"
    e <- expr
    m_reserved "do"
    s <- statement
    return $ AST.While e s

pl0 :: Parser AST.PL0
pl0 = program

pl0Parser = parseFromFile pl0
