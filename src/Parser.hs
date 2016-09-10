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
                                   "not", "and", "or"]
              , reservedNames = ["array", "begin", "case",
                                 "const", "div", "do", "downto",
                                 "else", "file", "for", "function",
                                 "goto", "if", "in", "label",
                                 "nil", "of", "packed", "procedure",
                                 "program", "record", "repeat", "set",
                                 "then", "to", "type", "until", "var",
                                 "while", "with"]
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
    <|> (m_reserved "True" >> return (AST.BConst True))
    <|> (m_reserved "False" >> return (AST.BConst False))
    <|> (m_integer >>= return . AST.IntConst)
    <|> (m_identifier >>= return . AST.Ident)

exprTable = [[prefix "not" AST.Not],
             [binary "*" AST.Mult AssocLeft, binary "/" AST.Div AssocLeft,
              binary "div" AST.DivT AssocLeft, binary "mod" AST.Mod AssocLeft],
             [binary "+" AST.Add AssocLeft, binary "-" AST.Sub AssocLeft],
             [binary "and" AST.And AssocLeft, binary "or" AST.Or AssocLeft],
             [binary "=" AST.Eq AssocLeft, binary "<>" AST.NE AssocLeft,
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
blocks =  try varDecPart
      <|>  begin

begin :: Parser AST.Pascal
begin = do m_reserved "begin"
           seq <- m_semiSep statements
           m_reserved "end"
           return $ AST.Begin seq

varDecPart :: Parser AST.Pascal
varDecPart = do try $ m_reserved "var"
                varDecs <- varDec `sepEndBy1` m_semi
                let varDecs' = concat varDecs
                return $ AST.VarDec varDecs'

varDec :: Parser [AST.Pascal]
varDec = do idents <- m_commaSep m_identifier
            m_colon
            t <- m_identifier
            let vars = map (flip AST.Var $ t) idents
            return vars

-- Holds non-if statements.
baseStatements :: Parser AST.Pascal
baseStatements = try assign
              <|> try begin

statements :: Parser AST.Pascal
statements = baseStatements
          <|> selectionStmnt

-- Handle ambiguous if statements.
statementElse :: Parser AST.Pascal
statementElse = baseStatements
             <|> ifElseStmnt'

-- Handle if statements.
selectionStmnt :: Parser AST.Pascal
selectionStmnt = try ifElseStmnt
              <|> ifStmnt

ifStmnt :: Parser AST.Pascal
ifStmnt = do m_reserved "if"
             pred <- expr
             m_reserved "then"
             thenBranch <- statements
             return $ AST.If pred thenBranch Nothing

ifElseStmnt :: Parser AST.Pascal
ifElseStmnt = do m_reserved "if"
                 pred <- expr
                 m_reserved "then"
                 thenBranch <- statementElse
                 m_reserved "else"
                 elseBranch <- statements
                 return $ AST.If pred thenBranch (Just elseBranch)

-- Handle ambiguous if statement.
ifElseStmnt' :: Parser AST.Pascal
ifElseStmnt' = do m_reserved "if"
                  pred <- expr
                  m_reserved "then"
                  thenBranch <- statementElse
                  m_reserved "else"
                  elseBranch <- statementElse
                  return $ AST.If pred thenBranch (Just elseBranch)

assign :: Parser AST.Pascal
assign = do ident <- m_identifier
            m_reservedOp ":="
            expr <- expr
            return $ AST.Assign ident expr

pascal :: Parser AST.Pascal
pascal = program

pascalParser :: String -> IO ()
pascalParser f = do
  result <- parseFromFile pascal f 
  case result of
    Left err   -> print err
    Right expr -> print expr

-- expr tester
exprParser :: String -> IO ()
exprParser s = case (parse expr "" s) of
                   Left err  -> print err
                   Right xs  -> print xs

