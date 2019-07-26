module IR where

type Label = String

data IR =
    JMP Label     |
    JMZ Label     |
    LOAD  Label   |
    STORE Label   |
    CALL  Label   |
    WRITE         |
    ADD           |
    SUB           |
    DIV           |
    MUL           |
    LT            |
    LTE           |
    GT            |
    GTE           |
    EQ            |
    NEQ           |
    HALT          |
    STARTFUNC     |
    RET           |
    DEC Integer   |
    NOOP
    deriving (Show)

data Line = Line (Maybe Label) IR
    deriving (Show)

mkLabel :: Integer -> String
mkLabel = ("s" ++) . show
