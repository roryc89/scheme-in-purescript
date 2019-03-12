module Main where

import Prelude

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Eval (primitiveBindings)
import Node.Process (argv)
import Node.SimpleRepl (simpleRepl)
import Repl (evalAndPrint, runRepl)

main :: Effect Unit
main = do
  args <- argv
  case args !! 2 of
    Nothing -> runRepl
    Just input -> do
      log $ "input: " <> input
      runOne input

runOne :: String -> Effect Unit
runOne expr = do
  startEnv <- primitiveBindings
  simpleRepl $ evalAndPrint startEnv expr
