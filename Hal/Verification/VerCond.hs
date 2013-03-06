{-# LANGUAGE RecordWildCards #-}
module Hal.Verification.VerCond where


-- | Imports de Hal
import Hal.Lang
import Hal.Verification.THoare
import Hal.Parser

-- | Imports de Equ
import qualified Equ.Expr as Equ
import qualified Equ.Theories.FOL as EquFol


data PartTHoare = PartTHoare { partPre :: FormFun
                             , partComm :: Maybe Comm
                             }

type THoares = [THoare]
type PartTHoares = [PartTHoare]

-- | Es una lista de comandos sin secuencia.
type LComm = [Comm]


commToList :: Comm -> LComm
commToList = commToList' []
    where commToList' :: LComm -> Comm -> LComm
          commToList' ls (Seq c1 c2) = (commToList' (commToList' ls c1) c2)
          commToList' ls c = ls++[c]

completeTHoare :: FormFun -> PartTHoare -> THoare
completeTHoare postc pth = 
            THoare { pre = partPre pth
               , comm = maybe Skip id (partComm pth)
               , post = postc
            }

         
addToPartTHoare :: Comm -> PartTHoare -> PartTHoare
addToPartTHoare c pth@PartTHoare{ partComm = pc } = pth { partComm = Just $ maybe c (flip Seq c) pc }

newPTHoare :: FormFun -> PartTHoare
newPTHoare f = PartTHoare { partPre = f
                          , partComm = Nothing
                          }
                          
addGuard :: BExp -> PartTHoare -> PartTHoare
addGuard b pth@(PartTHoare f _) = pth { partPre = EquFol.and f (bExpToFun b) }


vc :: THoares -> PartTHoares -> Comm -> (THoares,PartTHoares)
vc cths pths (Assert f) = 
    (cths ++ (map (completeTHoare f) pths),[newPTHoare f])
vc cths pths (If b c1 c2) = (cths++cthsB++cthsNB,pthsB++pthsNB)
    where (cthsB,pthsB) = vc2 [] (map (addGuard b) pths) (commToList c1) Nothing
          (cthsNB,pthsNB) = vc2 [] (map (addGuard (BUOp Not b)) pths) (commToList c2) Nothing
-- | Las ternas de Hoare en el Do son 3:
--   { Pre Skip Inv }
--   { (Inv && not b) Skip Pos }
--   { (Inv && b) c Inv }
vc cths pths (Do inv b c) = 
    (cths ++ t1 ++ cths',pths'++[pth])
    
    where t1 = map (completeTHoare inv) pths
          (cths',pths') = vc2 [] 
                             [PartTHoare (EquFol.and inv (bExpToFun b)) Nothing]
                             (commToList c)
                             (Just inv)
          pth = PartTHoare (EquFol.and inv (bExpToFun $ BUOp Not b)) Nothing
          
-- Skip, Abort, Assign
vc cths pths c = (cths,map (addToPartTHoare c) pths)


vc2 :: THoares -> PartTHoares -> LComm -> Maybe FormFun -> (THoares,PartTHoares)
vc2 cths pths [] mpostc = 
    maybe (cths,pths) 
          (\postc -> (cths++(map (completeTHoare postc) pths),[]))
          mpostc
vc2 cths pths (c:lcomms) mpostc = vc2 cths' pths' lcomms mpostc
    where (cths',pths') = vc cths pths c
          

          
          
          
verConditions :: Program -> THoares
verConditions (Prog _ pre c postc) = cths
    where (cths,pths) = vc2 [] 
                            [PartTHoare { partPre = pre
                                           , partComm = Nothing} ]
                            (commToList c)
                            (Just postc)

