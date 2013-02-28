{- | Evaluador del lenguaje Hal
    
    Para evaluar asumimos el programa typechekeo sin problemas.
-}
{-# LANGUAGE RecordWildCards #-}
module Hal.Eval where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT,get,put,execStateT)
import Control.Monad.Fix (fix)
import System.IO

import qualified Data.List as L
import Data.Maybe

-- Imports de Hal
import Hal.Lang
import Hal.Parser

-- | Elemento de un estado. Representa el valor de una variable en un momento
-- de la evaluación.
data StateTuple = IntVar  Identifier (Maybe Int)
                | BoolVar Identifier (Maybe Bool)
    deriving Show

instance Eq StateTuple where
    (IntVar i _) == (IntVar i' _) = i == i'
    (BoolVar i _) == (BoolVar i' _) = i == i'
    _ == _ = False

-- | Estado de la evaluación.
type State = [StateTuple]

-- | Mónada de la semántica denotacional.
type ProgState = StateT State IO

-- | Evaluador de los operadores binarios enteros.
evalIntBOp :: IntBOp -> ProgState Int -> ProgState Int -> ProgState Int
evalIntBOp Plus   = liftA2 (+)
evalIntBOp Times  = liftA2 (*)
evalIntBOp Substr = liftA2 (-)
evalIntBOp Div    = liftA2 div
evalIntBOp Mod    = liftA2 mod

-- | Evaluador de los operadores binarios boleanos.
evalBoolBOp :: BoolBOp -> ProgState Bool -> ProgState Bool -> ProgState Bool
evalBoolBOp And = liftA2 (&&)
evalBoolBOp Or  = liftA2 (||)

-- | Evaluador de los operadores unarios boleanos.
evalBoolUOp :: BoolUOp -> ProgState Bool -> ProgState Bool
evalBoolUOp Not = fmap not

-- | Evaluador de las relaciones binarias.
evalRelOp :: (Eq a, Ord a) => 
             RelOp -> ProgState a -> ProgState a -> ProgState Bool
evalRelOp Equal  = liftA2 (==)
evalRelOp Lt     = liftA2 (<)
evalRelOp Gt     = liftA2 (>)
evalRelOp NEqual = liftA2 (/=)

-- | Evaluador de expresiones enteras.
evalExp :: Exp -> ProgState Int
evalExp (IBOp iop e e') = evalIntBOp iop (evalExp e) (evalExp e')
evalExp (ICon i)  = return i
evalExp (IntId i) = get >>= \st -> 
                    maybe (error "Impossible")
                          stGetIntValue $ L.find (==(IntVar i Nothing)) st
    where
        getNewValue :: ProgState Int
        getNewValue = liftIO (putStr ("Ingrese la variable "++show (idName i) ++": ")) >>
                      liftIO (hFlush stdout) >> liftIO getLine >>= \str -> 
                      case parseConFromString str of
                          Left er -> liftIO (putStrLn "Valor no valido, intente de nuevo.") >> getNewValue
                          Right v -> do st <- get
                                        v' <- evalExp v
                                        let st' = L.map (updateValue i (Right v')) st
                                        put st'
                                        return v'
        stGetIntValue :: StateTuple -> ProgState Int
        stGetIntValue (IntVar _ mv) = maybe getNewValue return mv

-- | Evaluador de expresiones boleanas.
evalBExp :: BExp -> ProgState Bool
evalBExp (BRel rop e e') = evalRelOp rop (evalExp e) (evalExp e')
evalBExp (BUOp bop e)    = evalBoolUOp bop $ evalBExp e
evalBExp (BBOp bop e e') = evalBoolBOp bop (evalBExp e) (evalBExp e')
evalBExp (BCon b) = return b
evalBExp (BoolId i) = get >>= \st -> 
                      maybe (error "Impossible")
                            stGetBoolValue $ L.find (==(BoolVar i Nothing)) st
    where
        getNewValue :: ProgState Bool
        getNewValue = liftIO (putStr ("Ingrese la variable "++show (idName i) ++": ")) >>
                      liftIO (hFlush stdout) >> liftIO getLine >>= \str -> 
                      case parseBConFromString str of
                          Left er -> liftIO (putStrLn "Valor no valido, intente de nuevo.") >> getNewValue
                          Right v -> do st <- get
                                        v' <- evalBExp v
                                        let st' = L.map (updateValue i (Left v')) st
                                        put st'
                                        return v'
        stGetBoolValue :: StateTuple -> ProgState Bool
        stGetBoolValue (BoolVar _ mv) = maybe getNewValue return mv

-- | Actualiza el valor de un identificador en una tupla del estado.
updateValue :: Identifier -> Either Bool Int -> StateTuple -> StateTuple
updateValue i (Left v) stt@(BoolVar i' _) = if i == i' 
                                                then (BoolVar i $ Just v)
                                                else stt
updateValue i (Right v) stt@(IntVar i' _) = if i == i' 
                                                then (IntVar i $ Just v)
                                                else stt
updateValue _ _ stt = stt

-- | Agrega un identificador con su valor al estado.
addValue :: Identifier -> Either Bool Int -> State -> State
addValue i (Left v) st  = st++[BoolVar i $ Just v]
addValue i (Right v) st = st++[IntVar i $ Just v]

-- | Evaluador de los comandos.
evalComm :: Comm -> ProgState ()
evalComm Skip = return ()
evalComm Abort = error "¿Este no es el comportamiento que debería tener? :D"
evalComm (Assert b) = return ()
evalComm (If b c c') = evalBExp b >>= \vb ->
                       if vb then evalComm c else evalComm c'
evalComm (IAssig a e) = do 
                        evalE <- evalExp e
                        st <- get 
                        let st' = L.map (updateValue a $ Right evalE) st
                        put st'
evalComm (BAssig a e) = do 
                        evalE <- evalBExp e
                        st <- get 
                        let st' = L.map (updateValue a $ Left evalE) st
                        put st'
evalComm (Seq c c') = evalComm c >> evalComm c'
evalComm (Do inv b c) = fix evalDo
    where
        evalDo :: ProgState () -> ProgState ()
        evalDo f = do
                   vb <- evalBExp b
                   if vb then (evalComm (Seq c (Assert inv))) >> f 
                         else return ()

-- | Evaluador de los programas.
evalProgram :: Program -> IO State
evalProgram (Prog vars pre comms post) = execStateT (evalComm comms) fillState
    where
        fillState :: State
        fillState = map makeVar vars
        makeVar :: Identifier -> StateTuple
        makeVar i@(Identifier {..}) = case idDataType of
                                        IntTy  -> IntVar  i Nothing
                                        BoolTy -> BoolVar i Nothing
