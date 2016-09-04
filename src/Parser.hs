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
              , reservedOpNames = [":=", "+", "-", "*", "mod", "/",
                                   "=", "<=", "<",
                                   ">=", ">", "<>",
                                   "not", "and", "or", "xor"]
              , reservedNames = ["if", "then", "else", "begin",
                                 "end", "program", "writeln",
                                 "var", "True", "False"]
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

boolExpr = buildExpressionParser boolExprTable boolTerm

boolTerm = m_parens boolExpr
        <|> (m_reserved "True" >> return AST.True)
        <|> (m_reserved "False" >> return AST.False)
        <|> (m_identifier >>= return . AST.Ident)

boolExprTable = [[prefix "not" AST.Not],
                 [binary "and" AST.And AssocLeft],
                 [binary "or" AST.Or AssocLeft],
                 [binary "=" AST.Eq AssocLeft,
                  binary "<" AST.LT AssocLeft, binary "<=" AST.LTE AssocLeft,
                  binary ">" AST.GT AssocLeft, binary ">=" AST.GTE AssocLeft]]

binary name fun assoc = Infix (do { m_reservedOp name; return fun }) assoc
prefix name fun = Prefix (do { m_reservedOp name; return fun })

program :: Parser AST.Pascal
program = do try $ m_reserved "program"
             name <- m_identifier
             m_semi
             source <- many1 blocks
             m_dot
             return $ AST.Program name source

blocks :: Parser AST.Pascal
blocks =  try varDec
      <|>  blockBegin

blockBegin :: Parser AST.Pascal
blockBegin = do m_reserved "begin"
                -- seq <- statements `sepEndBy1` m_semi
                seq <- many1 statements
                m_reserved "end"
                return $ AST.Begin seq

varDec :: Parser AST.Pascal
varDec = do try $ m_reserved "var"
            vars <- var `sepEndBy1` m_semi
            return $ AST.VarDec vars

var :: Parser AST.Pascal
var = do ident <- m_identifier
         m_colon
         t <- m_identifier
         return $ AST.Var ident t

statements :: Parser AST.Pascal
statements = ifStmnt <|> assign

ifStmnt :: Parser AST.Pascal
ifStmnt = do m_reserved "if"
             pred <- boolExpr
             m_reserved "then"
             thenBranch <- statements
             elseBranch <- optionMaybe elsePart
             return $ AST.If pred thenBranch elseBranch
    where
        elsePart :: Parser AST.Pascal
        elsePart = do m_reserved "else"
                      stmnt <- statements
                      return stmnt

assign :: Parser AST.Pascal
assign = do ident <- m_identifier
            m_reservedOp ":="
            expr <- boolExpr
            m_semi
            return $ AST.Assign ident expr

pascal :: Parser AST.Pascal
pascal = program

pascalParser :: String -> IO ()
pascalParser f = do
  result <- parseFromFile pascal f 
  case result of
    Left err   -> print err
    Right expr -> print expr

-- boolExpr tester
boolParser :: String -> IO ()
boolParser s = case (parse boolExpr "" s) of
                   Left err  -> print err
                   Right xs  -> print xs

