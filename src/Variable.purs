module Variable where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), lookup)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Error (LispError(..), ThrowsError, extractValue, trapError)
import LispVal (LispVal)

type Env = Ref EnvVal

type EnvVal = Array (Tuple String (Ref LispVal))

nullEnv :: Effect Env
nullEnv = Ref.new []

type EffThrowsError = ExceptT LispError Effect

liftThrows :: forall a. ThrowsError a -> EffThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = pure val

runEffThrows :: EffThrowsError String -> Effect String
runEffThrows action = runExceptT (trapError action) >>= pure <<< extractValue

isBound :: Env -> String -> Effect Boolean
isBound envRef var = Ref.read envRef >>= pure <<< maybe false (const true) <<< lookup var

getVar :: Env -> String -> EffThrowsError LispVal
getVar envRef var = do
   env <- liftEffect $ Ref.read envRef
   maybe
     (throwError $ UnboundVar "Getting an unbound variable" var)
     (liftEffect <<< Ref.read)
     (lookup var env)

setVar :: Env -> String -> LispVal -> EffThrowsError LispVal
setVar envRef var value = do
  env <- liftEffect $ Ref.read envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftEffect <<< (Ref.write value))
    (lookup var env)

  pure value

defineVar :: Env -> String -> LispVal -> EffThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftEffect $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value *> pure value
     else liftEffect $ do
       valueRef <- Ref.new value
       env <- Ref.read envRef
       Ref.write ((Tuple var valueRef) : env) envRef
       pure value

bindVars :: Env -> Array (Tuple String LispVal) -> Effect Env
bindVars envRef bindings =
  Ref.read envRef >>= extendEnv bindings >>= Ref.new
  where
    extendEnv bindings_ env =
      liftM1 ((<>) env) (traverse addBinding bindings_)

    addBinding (Tuple var value) = do
      ref <- Ref.new value
      pure (Tuple var ref)

-- DEBUG

getEnvVars :: EnvVal -> Effect (Array (Tuple String LispVal))
getEnvVars = traverse \(Tuple str ref) -> do
  val <- Ref.read ref
  pure $ Tuple str val

showEnv :: EnvVal -> Effect String
showEnv = getEnvVars >>> map show
