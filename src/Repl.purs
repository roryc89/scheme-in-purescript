module Repl where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Eval (eval, primitiveBindings)
import LispVal (Env, liftThrows, runEffThrows)
import Node.SimpleRepl (Repl, putStrLn, readLine, simpleRepl)
import Parse (readExpr)

evalString ::  Env -> String -> Repl String
evalString env expr =
  liftEffect $ runEffThrows $ liftM1 show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> Repl Unit
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: forall m a. Monad m => (a -> Boolean) -> m a -> (a -> m Unit) -> m Unit
until_ pred prompt action = do
   result <- prompt
   if pred result
      then pure unit
      else action result *> until_ pred prompt action

runRepl :: Effect Unit
runRepl = do
  log "enter quit to exit"
  startEnv <- primitiveBindings
  simpleRepl (until_ ((==) "quit") readLine (evalAndPrint startEnv))
