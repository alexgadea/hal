{-# LANGUAGE RecordWildCards #-}
module Hal.Eval where

import Control.Monad.Fix (fix)

import qualified Data.List as L
import Data.Maybe

import Hal.Lang

-- Elemento de un estado. Representa el valor de una variable en un momento
-- de la evaluación.
data StateTuple = IntVar  Identifier (Maybe Int)
                | BoolVar Identifier (Maybe Bool)
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
evalOp (BOp Not)   [(BoolVal b)]              = BoolVal $ not b
evalOp (IOp Plus)  [(IntVal i),(IntVal i')]   = IntVal  $ i + i'
evalOp (ROp Equal) [(IntVal i),(IntVal i')]   = BoolVal $ i == i'
evalOp (ROp Equal) [(BoolVal i),(BoolVal i')] = BoolVal $ i == i'

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
                                 L.find (==(IntVar i Nothing)) st
                        BoolTy -> BoolVal $ stGetBoolValue $ fromJust $ 
                                  L.find (==(BoolVar i Nothing)) st
        
        stGetBoolValue :: StateTuple -> Bool
        stGetBoolValue (BoolVar _ v) = fromJust v
    
        stGetIntValue :: StateTuple -> Int
        stGetIntValue (IntVar _ v) = fromJust v

updateValue :: Identifier -> SemExpr -> StateTuple -> StateTuple
updateValue i (BoolVal v) stt@(BoolVar i' _) = if i == i' 
                                                then (BoolVar i $ Just v)
                                                else stt
updateValue i (IntVal v) stt@(IntVar i' _) = if i == i' 
                                                then (IntVar i $ Just v)
                                                else stt
addValue :: Identifier -> SemExpr -> State -> State
addValue i (BoolVal v) st = st++[BoolVar i $ Just v]
addValue i (IntVal v) st = st++[IntVar i $ Just v]

evalComm :: Comm -> State -> State
evalComm Skip st = st
evalComm Abort st = error "¿Este no es el comportamiento que debería tener? :D"
evalComm (Assert b) st = let BoolVal vb = evalExpr b st
                         in if vb then st else error "Assert falso"
evalComm (If b c c') st = let BoolVal vb = evalExpr b st
                          in if vb then evalComm c st else evalComm c' st
evalComm (Assig a e) st = let evalE = evalExpr e st 
                              evalA = evalAcc a st
                          in
                          L.map (updateValue evalA evalE) st
evalComm (Seq c c') st = evalComm c' $ evalComm c st
evalComm (Do inv b c) st = let BoolVal vinv = evalExpr inv st
                           in if vinv
                                then fix evalDo st
                                else error "Invariante falso"
    where
        evalDo :: (State -> State) -> (State -> State)
        evalDo f st = let BoolVal vb = evalExpr b st
                      in if vb then f (evalComm (Seq c (Assert inv)) st) else st

evalProgram :: Program -> State
evalProgram (Prog vars comms) = evalComm comms fillState
    where
        fillState :: State
        fillState = map makeVar vars
        makeVar :: Identifier -> StateTuple
        makeVar i@(Identifier {..}) = case idDataType of
                                        IntTy  -> IntVar  i Nothing
                                        BoolTy -> BoolVar i Nothing
