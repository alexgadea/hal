module Hal.Eval where

import Control.Monad.Fix (fix)

import qualified Data.List as L
import Data.Maybe

import Hal.Lang

-- Elemento de un estado. Representa el valor de una variable en un momento
-- de la evaluación.
data StateTuple = IntVar  Identifier Int
                | BoolVar Identifier Bool
    deriving Show

instance Eq StateTuple where
    (IntVar i _) == (IntVar i' _) = i == i'
    (BoolVar i _) == (BoolVar i' _) = i == i'
    _ == _ = False

-- Estado de la evaluación.
type State = [StateTuple]

-- Valor Semántico de una expresión del lenguaje.
data SemExpr = IntVal Int
             | BoolVal Bool
    deriving Show

-- Para evaluar asumimos el programa typechekeo sin problemas.

evalOp :: OpName -> [SemExpr]-> SemExpr
evalOp Not   [(BoolVal b)]              = BoolVal $ not b
evalOp Plus  [(IntVal i),(IntVal i')]   = IntVal  $ i + i'
evalOp Equal [(IntVal i),(IntVal i')]   = BoolVal $ i == i'
evalOp Equal [(BoolVal i),(BoolVal i')] = BoolVal $ i == i'

evalAcc :: Acc -> State -> Identifier
evalAcc (IdAcc i) st = i
        
evalExpr :: Expr -> State -> SemExpr
evalExpr (ICon i) _ = IntVal i
evalExpr (BCon b) _ = BoolVal b
evalExpr (Op op exprs) st = evalOp (opName op) $ L.map (flip evalExpr st) exprs
evalExpr (IdExpr i) st = getIdValue
    where
        getIdValue :: SemExpr
        getIdValue = case idDataType i of
                        IntTy -> IntVal $ stGetIntValue $ fromJust $ 
                                 L.find (==(IntVar i 0)) st
                        BoolTy -> BoolVal $ stGetBoolValue $ fromJust $ 
                                  L.find (==(BoolVar i True)) st
        
        stGetBoolValue :: StateTuple -> Bool
        stGetBoolValue (BoolVar _ v) = v
    
        stGetIntValue :: StateTuple -> Int
        stGetIntValue (IntVar _ v) = v

updateValue :: Identifier -> SemExpr -> StateTuple -> StateTuple
updateValue i (BoolVal v) stt@(BoolVar i' _) = if i == i' 
                                                then (BoolVar i v)
                                                else stt
updateValue i (IntVal v) stt@(IntVar i' _) = if i == i' 
                                                then (IntVar i v)
                                                else stt
addValue :: Identifier -> SemExpr -> State -> State
addValue i (BoolVal v) st = st++[BoolVar i v]
addValue i (IntVal v) st = st++[IntVar i v]

evalComm :: Comm -> State -> State
evalComm Skip st = st
evalComm Abort st = error "¿Este no es el comportamiento que debería tener? :D"
evalComm (If b c c') st = let BoolVal vb = evalExpr b st
                          in if vb then evalComm c st else evalComm c' st
evalComm (NewVar i e c) st = let evalE = evalExpr e st
                             in
                                evalComm c (addValue i evalE st)
evalComm (Assig a e) st = let evalE = evalExpr e st 
                              evalA = evalAcc a st
                          in
                          L.map (updateValue evalA evalE) st
evalComm (Seq c c') st = evalComm c' $ evalComm c st
evalComm (Do b c) st = fix evalDo st
    where
        evalDo :: (State -> State) -> (State -> State)
        evalDo f st = let BoolVal vb = evalExpr b st
                      in if vb then f (evalComm c st) else st
