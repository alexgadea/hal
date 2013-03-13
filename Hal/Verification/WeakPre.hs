module Hal.Verification.WeakPre where

import Hal.Verification.THoare
import Hal.Verification.VerCond
import Hal.Lang
import Hal.Parser(parseFromString,prg4,parseFromFile)

import qualified Equ.PreExpr as PE
import qualified Equ.Theories.FOL as FOL
import Equ.Expr
import Equ.Types

import qualified Data.Map as M


proofObligations :: Program -> [FormFun]
proofObligations = (map weakp) . verConditions 

-- | Función que calcula la wakest precondition de un comando y una post-condición
weakp :: THoare -> FormFun
weakp th = FOL.impl (pre th) $ wp (comm th) (post th)


wp :: Comm -> FormFun -> FormFun
wp Skip f = f
wp Abort _ = FOL.false
wp (IAssig i e) (Expr f) = Expr $ PE.applySubst f subst
    where subst = M.fromList [(vari,expToFun e)]
          vari = PE.var (idName i) (TyAtom ATyInt)
wp (BAssig b e) (Expr f) = Expr $ PE.applySubst f subst
    where subst = M.fromList [(varb,bExpToFun' e)]
          varb = PE.var (idName b) (TyAtom ATyBool)
wp (Seq c1 c2) f = wp c1 (wp c2 f)


-- | SOLO PARA PROBARRRR
ej1 =
    let Right p = parseFromString (unlines prg4) in
        map weakp $ verConditions p

ej2 = parseFromFile "Examples/div.lisa" >>= \prg ->
      putStrLn $ show $ map weakp $ verConditions prg

