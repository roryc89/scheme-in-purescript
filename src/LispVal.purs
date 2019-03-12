module LispVal where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), lookup)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import LispVal (LispVal)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (ParseError)
import Utils (unwords)

data LispVal
  = Atom String
  | List (List LispVal)
  | DottedList (List LispVal) LispVal
  | Number Number
  | String String
  | Bool Boolean
  | PrimitiveFunc (List LispVal -> ThrowsError LispVal)
  | Func
      { params :: List String
      , vararg :: Maybe String
      , body :: List LispVal
      , closure :: Env
      }

showVal :: LispVal -> String
showVal (String contents) = "\"" <> contents <> "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool b) = show b
showVal (List contents) = "(" <> unwords contents <> ")"
showVal (DottedList head tail) = "(" <> unwords head <> " . " <> showVal tail <> ")"
showVal (PrimitiveFunc _) = "<primitive>"

showVal (Func {params, vararg, body, closure}) =
  "(lamda (" <> unwords params <>
    (case vararg of
      Nothing -> ""
      Just arg -> " . " <> arg) <> ") ...)"


derive instance genericLispVal :: Generic LispVal _
-- derive instance eqLispVal :: Eq LispVal

instance showLispVal :: Show LispVal where
  show = showVal
  -- show (Atom a) = "(Atom " <> show a <> ")"
  -- show (List a) = "(List " <> show a <> ")"
  -- show (DottedList a _) = "(DottedList " <> show a <> ")"
  -- show (Number a) = "(Number " <> show a <> ")"
  -- show (String a) = "(String " <> show a <> ")"
  -- show (Bool a) = "(Bool " <> show a <> ")"


data LispError
  = NumArgs Int (List LispVal)
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message <> ": " <> varname
showError (BadSpecialForm message form) = message <> ": " <> show form
showError (NotFunction message func)    = message <> ": " <> show func
showError (NumArgs expected found)      = "Expected " <> show expected
                                       <> " args; found values " <> unwords found
showError (TypeMismatch expected found) = "Invalid type: expected " <> expected
                                       <> ", found " <> show found
showError (Parser parseErr)             = "Parse error at " <> show parseErr
showError (Default _)             = "Default error at "

instance showLispError :: Show LispError where show = showError

type ThrowsError = Either LispError

trapError :: forall e m. MonadError e m => Show e => m String -> m String
trapError action = catchError action (pure <<< show)

extractValue :: forall a. ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = unsafeCrashWith "Cannot extractValue from Left constructor"

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
