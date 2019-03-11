module Error where

import Prelude
import Control.Monad.Except (class MonadError, catchError)
import Data.Either (Either(..))
import Data.List (List)
import LispVal (LispVal)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (ParseError)
import Utils (unwords)

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
