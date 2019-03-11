module Repl where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Error (extractValue, trapError)
import Eval (eval)
import Node.SimpleRepl (Repl, putStrLn, readLine, simpleRepl)
import Parse (readExpr)

readPrompt :: String -> Repl String
readPrompt str = putStrLn str *> readLine

evalString :: String -> Repl String
evalString expr = pure $ extractValue $ trapError (liftM1 show $ readExpr expr >>= eval)

evalAndPrint :: String -> Repl Unit
evalAndPrint expr = evalString expr >>= (putStrLn)

until_ :: forall m a. Monad m => (a -> Boolean) -> m a -> (a -> m Unit) -> m Unit
until_ pred prompt action = do
   result <- prompt
   if pred result
      then pure unit
      else action result *> until_ pred prompt action

runRepl :: Effect Unit
runRepl = do
  log "enter quit to exit"
  simpleRepl (until_ ((==) "quit") readLine evalAndPrint)
