-- | Módulo de la sintáxis lenguaje imperativo simple con anotaciones (LISA).
{-# Language GADTs #-}
module Hal.Lang where

import qualified Data.Text as T

import Equ.Expr
import qualified Equ.PreExpr as PE
import qualified Equ.Types as ETy
import qualified Equ.Theories.FOL as ETheoriesF
import qualified Equ.Theories.Arith as ETheoriesA

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

-- Expresiones boleanas.
data BExp where
    BoolId :: Identifier -> BExp
    BCon   :: Bool -> BExp
    
    BBOp :: BoolBOp -> BExp -> BExp -> BExp
    BUOp :: BoolUOp -> BExp -> BExp
    
    BRel :: RelOp -> Exp -> Exp -> BExp
   
   deriving Show

-- Los terminos que representan los comandos.
data Comm where
    Skip  :: Comm
    Abort :: Comm
    
    Assert :: FormFun -> Comm
    
    If     :: BExp -> Comm -> Comm -> Comm
    
    IAssig :: Identifier -> Exp -> Comm
    BAssig :: Identifier -> BExp -> Comm
    
    Do     :: FormFun -> BExp -> Comm -> Comm
    Seq    :: Comm -> Comm -> Comm
    
    deriving Show

-- Un programa se separa en dos partes principales, la declaración de las
-- variables y los comandos en sí que conforman el programa.
data Program where
    Prog :: LIdentifier -> FormFun -> Comm -> FormFun -> Program
    deriving Show
    
funBoolType = ETy.TyAtom ETy.ATyBool

funNatType = ETy.TyAtom ETy.ATyNat
    
    
bExpToFun = Expr . bExpToFun'
    
bExpToFun' :: BExp -> PE.PreExpr
bExpToFun' (BoolId i) = PE.Var $ PE.Variable (idName i) funBoolType
bExpToFun' (BCon c) = PE.Con $ 
                     if c
                        then ETheoriesF.folTrue
                        else ETheoriesF.folFalse
bExpToFun' (BBOp op e1 e2) = 
    case op of
        And -> PE.BinOp ETheoriesF.folAnd (bExpToFun' e1) (bExpToFun' e2)
        Or -> PE.BinOp ETheoriesF.folOr (bExpToFun' e1) (bExpToFun' e2)
bExpToFun' (BUOp Not e) = 
    PE.UnOp ETheoriesF.folNeg (bExpToFun' e)
bExpToFun' (BRel rel e1 e2) = 
    case rel of
        Equal -> PE.BinOp ETheoriesF.folEqual (expToFun e1) (expToFun e2)
        Lt -> PE.BinOp ETheoriesA.lessOper (expToFun e1) (expToFun e2)
    
expToFun :: Exp -> PE.PreExpr
expToFun (IntId i) = PE.Var $ PE.Variable (idName i) funNatType
expToFun (ICon i) = intToFun i
    where intToFun i = if i<0
                        then PE.UnOp ETheoriesA.natNegNum $ expToFun (ICon i)
                        else case i of
                              0 -> PE.Con ETheoriesA.natZero
                              n -> PE.UnOp ETheoriesA.natSucc $ intToFun $ n-1
expToFun (IBOp op e1 e2) =
    case op of
         Plus -> PE.BinOp ETheoriesA.natSum (expToFun e1) (expToFun e2)
         Times -> PE.BinOp ETheoriesA.natProd (expToFun e1) (expToFun e2)
         Substr -> PE.BinOp ETheoriesA.natDif (expToFun e1) (expToFun e2)
         Div -> PE.BinOp ETheoriesA.natDiv (expToFun e1) (expToFun e2)
         Mod -> PE.BinOp ETheoriesA.natMod (expToFun e1) (expToFun e2)


         