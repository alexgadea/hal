module Hal.Interpreter.Interpreter where

-- Imports Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT,get,put,runStateT)

-- Imports de Hal
import Hal.Lang
import Hal.Evaluation.Eval

import Hal.Parser

data IState = IState { executedTracePrg  :: Maybe Comm
                     , nexecutedTracePrg :: Maybe Comm
                     , prgState          :: State
                     }

instance Show IState where
    show (IState (Just c) (Just c') st) = unlines [ show c
                                                  , ""
                                                  , show st
                                                  , ""
                                                  , show c'
                                                  ]
    show (IState Nothing (Just c') st) = unlines [ show st
                                                 , ""
                                                 , show c'
                                                 ]
    show (IState (Just c) Nothing st) = unlines [ show c
                                                , ""
                                                , show st
                                                ]
    show (IState Nothing Nothing st) = show st

initIState :: IState
initIState = IState Nothing Nothing initState
                     
-- | Mónada del estado del interprete.
type InterpreterState = StateT IState IO

-- | Comandos aceptados por el interprete.
data ICommand = Load Program 
              | LoadFile String 
              | Restart
              | Step 
              | View 
              | Exit
    deriving Show

evalInterpreter :: ICommand -> InterpreterState ()
evalInterpreter (Load (Prog vars _ c _)) = 
                do
                let newIState = IState Nothing (Just c) (fillState vars)
                put newIState
evalInterpreter Restart = do
                istate <- get
                let exec  = executedTracePrg istate
                    nexec = nexecutedTracePrg istate
                    mc     = case (exec,nexec) of
                                (Just c,Just c') -> Just (Seq c c')
                                (Just c,_)       -> Just c
                                (_,Just c')      -> Just c'
                                (_,_)            -> Nothing
                    vars  = takeIdentifiers $ prgState istate
                    newIState = IState Nothing mc (fillState vars)
                put newIState    
evalInterpreter Step = do
                       istate <- get
                       case istate of
                            (IState _ (Just c) st) -> 
                                liftIO (runStateT (evalStepComm c) st) >>= 
                                \(mc,st') -> updateState istate mc st'
                            (IState _ Nothing _) -> liftIO (putStrLn "Nada que evaluar")
evalInterpreter View = do
                        istate <- get
                        liftIO (putStrLn $ "\n\n" ++ show istate ++ "\n\n")

updateState :: IState -> (Maybe Comm,Maybe Comm) -> State -> InterpreterState ()
updateState istate (mc,mc') st = 
        case (istate,mc) of
            (IState Nothing _ _,_) -> put $ IState mc mc' st
            (IState (Just exec) _ _,Just c) -> put $ IState (Just $ Seq exec c) mc' st
            (IState (Just exec) _ _,Nothing) -> put $ IState (Just exec) mc' st
