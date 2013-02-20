module Hal.TypeCheck where

import qualified Control.Monad.State as MS
import qualified Data.List as L

import Hal.Lang

-- Los errores de tipeo.
data TypeCheckError = TCError
    deriving Show

-- Contexto para el checkeo de tipos.
type Ctx = [Identifier]

-- El estado del chequeo de tipos.
type TypeCheckState = [TypeCheckError]

-- La mónada para el chequeo de tipos.
type TypeCheck = MS.StateT TypeCheckState IO

dtypeUnify :: DataType -> DataType -> Bool
dtypeUnify = (==)

typeCheckAcc :: Acc -> Ctx -> TypeCheck (Maybe DataType)
typeCheckAcc (IdAcc i) ctx = 
            if idType i == IdVar 
                then return $ maybe (error "Impossible") 
                                    (return . idDataType) $ L.find (==i) ctx
                else typeCheckPutErr TCError >> return Nothing

typeCheckExpr :: Expr -> Ctx -> TypeCheck (Maybe DataType)
typeCheckExpr (IdExpr i) ctx = return $ maybe (error "Impossible") 
                                              (Just . idDataType) $ L.find (==i) ctx
typeCheckExpr (ICon _) ctx = return $ Just IntTy
typeCheckExpr (BCon _) ctx = return $ Just BoolTy
typeCheckExpr (Op op args) ctx = checkTypeArgs (opType op) args
    where
        checkTypeArgs :: DataType -> [Expr] -> TypeCheck (Maybe DataType)
        checkTypeArgs dt [] = return $ Just dt
        checkTypeArgs (dt :-> dtt) (e:es) = typeCheckExpr e ctx >>= \mdt ->
                case mdt of
                    Nothing -> return Nothing
                    Just edt -> MS.unless (dtypeUnify dt edt) (typeCheckPutErr TCError) >> 
                                checkTypeArgs dtt es
        checkTypeArgs _ (e:es) = MS.get >>= \errs -> MS.put (errs ++ [TCError]) >> 
                                 return Nothing

typeCheckPutErr :: TypeCheckError -> TypeCheck ()
typeCheckPutErr er = MS.get >>= \errs -> MS.put (errs ++ [er])

tcTypeCheck :: Maybe DataType -> DataType -> 
               TypeCheckError -> TypeCheck () -> TypeCheck ()
tcTypeCheck mdt dt err tc = case mdt of
                                Nothing -> return ()
                                Just dt' -> if dtypeUnify dt dt'
                                                then tc
                                                else typeCheckPutErr err

typeCheckComm :: Comm -> Ctx -> TypeCheck ()
typeCheckComm Skip ctx = return ()
typeCheckComm Abort ctx = return ()
typeCheckComm (If b c c') ctx = typeCheckExpr b ctx >>= \mdt ->
                                tcTypeCheck mdt BoolTy TCError 
                                                (typeCheckComm c ctx >>
                                                 typeCheckComm c' ctx)
typeCheckComm (NewVar i e c) ctx = typeCheckExpr e ctx >>= \mdt ->
                                   tcTypeCheck mdt (idDataType i) TCError (typeCheckComm c ctx)
typeCheckComm (NewCon i e c) ctx = typeCheckExpr e ctx >>= \mdt ->
                                   tcTypeCheck mdt (idDataType i) TCError (typeCheckComm c ctx)
typeCheckComm (Assig a e) ctx = 
                typeCheckExpr e ctx >>= \mdt ->
                typeCheckAcc a ctx >>= \madt ->
                case (mdt,madt) of
                    (Nothing,_) -> return ()
                    (_,Nothing) -> return ()
                    (Just edt,Just adt) -> if dtypeUnify adt edt
                                            then return ()
                                            else MS.get >>= \errs -> 
                                                 MS.put (errs ++ [TCError])
typeCheckComm (Do b c) ctx = typeCheckExpr b ctx >>= \mdt ->
                             tcTypeCheck mdt BoolTy TCError (typeCheckComm c ctx)
typeCheckComm (Seq c c') ctx = typeCheckComm c ctx >> typeCheckComm c' ctx 
