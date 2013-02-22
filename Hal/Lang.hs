{-# Language GADTs #-}
module Hal.Lang where

import qualified Data.Text as T

infixr 9 :->

type FormFun = String

type LIdentifier = [Identifier]

data AtomTy = IntTy | BoolTy
    deriving Eq

-- Tipo de dato básico para las expresiones.
data Type = ATy AtomTy | (:->) Type Type
    deriving Eq

data IdType = IsVar | IsCon
    deriving Eq
    
-- Identificador de variable y constante.
data Identifier = Identifier { idName     :: T.Text
                             , idDataType :: AtomTy
                             , idType     :: IdType
                             }
                             
instance Show Identifier where
    show (Identifier i _ _) = T.unpack i
                             
instance Eq Identifier where
    i == i' = idName i == idName i'

data BoolOp = And | Or | Not

data IntOp = Plus | Times | Substr | Div

data RelOp = Equal | Lt | Gt
    
data OpName = BOp BoolOp | IOp IntOp | ROp RelOp

-- Tipo de dato generico de un operador.
data Operator = Operator { opName :: OpName
                         , opRepr :: T.Text
                         , opType :: Type
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

-- Los terminos que representan los comandos.
data Comm where
    Skip  :: Comm
    Abort :: Comm
    
    -- De momento tenemos dos versiones de asserts como comando.
    -- Una usando las expresiones propias del lenguaje y otra usando
    -- fórmulas de fun.
    Assert  :: Expr -> Comm
    Assert' :: FormFun -> Comm
    
    If    :: Expr -> Comm -> Comm -> Comm
    Assig :: Acc  -> Expr -> Comm
    Do    :: Expr -> Expr -> Comm -> Comm
    Seq   :: Comm -> Comm -> Comm

-- Un programa se separa en dos partes principales, la declaración de las
-- variables y los comandos en sí que conforman el programa.
data Program where
    Prog :: LIdentifier -> Comm -> Program
