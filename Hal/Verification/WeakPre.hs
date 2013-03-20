module Hal.Verification.WeakPre where

import Hal.Verification.THoare
import Hal.Verification.VerCond
import Hal.Lang
import Hal.Parser(parseFromString,parseFromFile)

import qualified Equ.PreExpr as PE
import qualified Equ.Theories.FOL as FOL
import Equ.Expr
import Equ.Types

import qualified Data.Map as M


proofObligations :: Program -> [FormFun]
proofObligations = (map weakp) . verConditions 


weakp :: THoare -> FormFun
weakp th = FOL.impl (pre th) $ wp (comm th) (post th)


-- | Función que calcula la wakest precondition de un comando y una post-condición
wp :: GuardComm -> FormFun -> FormFun
wp [NGuard Skip] f = f
wp [NGuard Abort] _ = FOL.false
wp [NGuard (IAssig i e)] (Expr f) = Expr $ PE.applySubst f subst
    where 
        subst :: PE.ExprSubst
        subst = M.fromList [(vari,expToFun e)]
        vari :: PE.Variable
        vari = PE.var (idName i) (TyAtom ATyInt)
wp [NGuard (BAssig b e)] (Expr f) = Expr $ PE.applySubst f subst
    where 
        subst :: PE.ExprSubst
        subst = M.fromList [(varb,bExpToFun' e)]
        varb :: PE.Variable
        varb = PE.var (idName b) (TyAtom ATyBool)
wp (c1:gs) f = case c1 of
                    Guard b -> FOL.impl (bExpToFun b) (wp gs f)
                    NGuard c -> wp [c1] (wp gs f)



-- | SOLO PARA PROBARRRR
ej s =
    let Right p = parseFromString (unlines s) in
        map weakp $ verConditions p

ej2 = parseFromFile "Examples/div.lisa" >>= \prg ->
      putStrLn $ show $ map weakp $ verConditions prg

