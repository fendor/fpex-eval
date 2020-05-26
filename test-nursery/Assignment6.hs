module Assignment6 where

type Identifier = String

type Program_Name = Identifier

type Variable = Identifier

type PN = Program_Name

data P = P PN [S] deriving (Eq, Show) -- P for program

data S
  = Ass E E -- S for statement
  | If PE [S] [S]
  | While PE [S]
  | Repeat [S] PE
  deriving (Eq, Show)

data E
  = I Integer -- E for expression
  | F Float
  | V Variable
  | Plu E E
  | Min E E
  | Mul E E
  | Div E E
  deriving (Eq, Show)

data PE
  = Equal E E -- PE for predicate expression
  | NEqual E E
  | GEqual E E
  | LEqual
  deriving (Eq, Show)

type Parse1 a b = [a] -> [(b, [a])]

parser1 :: Parse1 Char P
parser1 = undefined

topLevel1 :: Parse1 a b -> [a] -> b
topLevel1 = undefined