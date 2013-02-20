{-# Language GADTs #-}
module Hal.Lang where

import qualified Data.Text as T

infixr 9 :->

-- Tipo de dato básico para las expresiones.
data DataType = IntTy | BoolTy | (:->) DataType DataType
    deriving Eq

data IdType = IdVar | IdCon
    deriving Eq
    
-- Identificador de variable y constante.
data Identifier = Identifier { idName     :: T.Text
                             , idDataType :: DataType
                             , idType     :: IdType
                             }
                             
instance Show Identifier where
    show (Identifier i _ _) = T.unpack i
                             
instance Eq Identifier where
    i == i' = idName i == idName i'

-- Nombre de operadores.
data OpName = Not | Plus | Equal

-- Tipo de dato generico de un operador.
data Operator = Operator { opName :: OpName
                         , opRepr :: T.Text
                         , opType :: DataType
                         }

-- Expresiones del lenguaje. Encapsulamos las expresiones aritmeticas 
-- ademas del identificador usado como valor.
data Expr where
    IdExpr :: Identifier -> Expr
    
    Op :: Operator -> [Expr] -> Expr
    
    ICon :: Int -> Expr
    BCon :: Bool -> Expr

-- De momento es para la representación del identificador como variable.
data Acc where
    IdAcc :: Identifier -> Acc

-- Los terminos que representan programas.
data Comm where
    Skip  :: Comm
    Abort :: Comm
    
    NewVar :: Identifier -> Expr -> Comm -> Comm
    NewCon :: Identifier -> Expr -> Comm -> Comm
    
    If    :: Expr -> Comm -> Comm -> Comm
    Assig :: Acc -> Expr -> Comm
    Do    :: Expr  -> Comm -> Comm
    Seq   :: Comm -> Comm -> Comm

-- ########################### Operadores ###########################
plus :: Operator
plus = Operator Plus (T.pack "+") (IntTy :-> IntTy :-> IntTy)

equal :: Operator
equal = Operator Equal (T.pack "==") (IntTy :-> IntTy :-> BoolTy)

notOp :: Operator
notOp = Operator Not (T.pack "--") (BoolTy :-> BoolTy)
