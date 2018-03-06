module Types

import Data.SortedMap

%access public export

total
ListFun : Type -> Type
ListFun a = List a -> a

data MalType = MInt Int
             | MSymbol String
             | MList (List MalType)
             | MFunc (ListFun MalType)

MalFunc : Type
MalFunc = ListFun MalType

MalEnv : Type
MalEnv = SortedMap String MalType

Show MalType where
  show (MInt n) = show n
  show (MSymbol str) = str
  show (MList xs) = "(" ++ unwords (map show xs) ++ ")"
  show (MFunc _) = "<fun>"
