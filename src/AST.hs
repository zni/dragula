module AST where

type Type = String

data Pascal =
    Program String [Pascal]       |
    Begin [Pascal]                |
    If Expr Pascal (Maybe Pascal) |
    While Expr Pascal             |
    VarDec [Pascal]               |
    Var String Type               |
    WriteLn String                |
    Assign String Expr            |
    Noop
    deriving (Show)

data Expr =
    Mult Expr Expr     |
    Div Expr Expr      |
    DivT Expr Expr     |
    Mod Expr Expr      |
    Add Expr Expr      |
    Sub Expr Expr      |
    LT Expr Expr       |
    LTE Expr Expr      |
    GT Expr Expr       |
    GTE Expr Expr      |
    NE Expr Expr       |
    Eq Expr Expr       |
    And Expr Expr      |
    Or Expr Expr       |
    Xor Expr Expr      |
    Not Expr           |
    Ident String       |
    BConst Bool        |
    IntConst Integer   |
    StringConst String
    deriving (Show)
