module AST where

type Type = String

data PL0 =
    Program PL0                             |
    Block (Maybe PL0) (Maybe PL0) [PL0] PL0 |
    Procedure String PL0                    |
    Begin [PL0]                             |
    If Expr PL0                             |
    While Expr PL0                          |
    VarDec [PL0]                            |
    ConstDec [PL0]                          |
    Var String                              |
    Const String Integer                    |
    WriteLn Expr                            |
    Call String                             |
    Assign String Expr
    deriving (Show)

data Expr =
    Mult Expr Expr     |
    Div Expr Expr      |
    Add Expr Expr      |
    Sub Expr Expr      |
    LT Expr Expr       |
    LTE Expr Expr      |
    GT Expr Expr       |
    GTE Expr Expr      |
    NE Expr Expr       |
    Eq Expr Expr       |
    Ident String       |
    IntConst Integer
    deriving (Show)
