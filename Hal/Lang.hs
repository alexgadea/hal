-- | Módulo de la sintáxis lenguaje imperativo simple con anotaciones (LISA).
{-# Language GADTs #-}
module Hal.Lang where

import qualified Data.Text as T

import Equ.Expr

type FormFun = Expr

type LIdentifier = [Identifier]

-- Tipo de dato básico para las expresiones.
data Type = IntTy | BoolTy
    deriving (Eq,Show)

-- Lo usamos para determinar si un identificador es una variables o una
-- constante.
data IdType = IsVar | IsCon
    deriving (Eq,Show)

-- Identificador de variable y constante.
data Identifier = Identifier { idName     :: T.Text
                             , idDataType :: Type
                             , idType     :: IdType
                             }
                             
instance Show Identifier where
    show (Identifier i _ _) = T.unpack i
                             
instance Eq Identifier where
    i == i' = idName i == idName i'

-- Operadores binarios boleanos.
data BoolBOp = And | Or 
    deriving Show

-- Operadores unarios boleanos.
data BoolUOp = Not
    deriving Show

-- Operadores binarios enteros.
data IntBOp = Plus | Times | Substr | Div | Mod
    deriving Show
    
-- Operadores unarios enteros
-- NO HAY

-- Relaciones binarias.
data RelOp = Equal | Lt | Gt | NEqual
   deriving Show

-- Expresiones enteras.
data Exp where
    IntId  :: Identifier -> Exp
    ICon   :: Int -> Exp
    
    IBOp :: IntBOp -> Exp -> Exp -> Exp
    
    deriving Show

-- Relaciones.
data Relation where
    Rel :: RelOp -> Exp -> Exp -> Relation
    
    deriving Show

-- Expresiones boleanas.
data BExp where
    BoolId :: Identifier -> BExp
    BCon   :: Bool -> BExp
    
    BBOp :: BoolBOp -> BExp -> BExp -> BExp
    BUOp :: BoolUOp -> BExp -> BExp
    
    BRel :: Relation -> BExp
   
   deriving Show

-- Aceptor entero.
data Acc where
    IntIdAcc :: Identifier -> Acc

-- Aceptor boleano.
data BAcc where
    BoolIdAcc :: Identifier -> BAcc
    
instance Show Acc where
    show (IntIdAcc i) = show i

instance Show BAcc where
    show (BoolIdAcc i) = show i

-- Los terminos que representan los comandos.
data Comm where
    Skip  :: Comm
    Abort :: Comm
    
    Assert :: FormFun -> Comm
    
    If     :: BExp -> Comm -> Comm -> Comm
    
    IAssig :: Acc  -> Exp -> Comm
    BAssig :: BAcc -> BExp -> Comm
    
    Do     :: FormFun -> BExp -> Comm -> Comm
    Seq    :: Comm -> Comm -> Comm
    
    deriving Show

-- Un programa se separa en dos partes principales, la declaración de las
-- variables y los comandos en sí que conforman el programa.
data Program where
    Prog :: LIdentifier -> FormFun -> Comm -> FormFun -> Program
    deriving Show