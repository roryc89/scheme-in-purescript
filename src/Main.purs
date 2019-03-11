module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Error (LispError(..), ThrowsError, extractValue, trapError)
import Eval (eval)
import LispVal (LispVal)
import Node.Process (argv)
import Parse (parseExpr)
import Text.Parsing.Parser (runParser)

main :: Effect Unit
main = do
  args <- argv
  case args !! 2 of
    Nothing -> log "No input provided"
    Just input -> do
      log $ "input: " <> input
      evaled <- pure $ liftM1 show $ readExpr input >>= eval
      logShow $ extractValue $ trapError evaled


readExpr :: String -> ThrowsError LispVal
readExpr input = case runParser input parseExpr of
  Left err -> throwError $ Parser err
  Right val -> pure val
