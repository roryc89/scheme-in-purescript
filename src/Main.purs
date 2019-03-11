module Main where

import Prelude

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Error (extractValue, trapError)
import Eval (eval)
import Node.Process (argv)
import Parse (readExpr)
import Repl (runRepl)

main :: Effect Unit
main = do
  args <- argv
  case args !! 2 of
    Nothing -> runRepl
    Just input -> do
      log $ "input: " <> input
      evaled <- pure $ liftM1 show $ readExpr input >>= eval
      logShow $ extractValue $ trapError evaled
