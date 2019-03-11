module LispVal where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Utils (unwords)


data LispVal
  = Atom String
  | List (List LispVal)
  | DottedList (List LispVal) LispVal
  | Number Number
  | String String
  | Bool Boolean

showVal :: LispVal -> String
showVal (String contents) = "\"" <> contents <> "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool b) = show b
showVal (List contents) = "(" <> unwords contents <> ")"
showVal (DottedList head tail) = "(" <> unwords head <> " . " <> showVal tail <> ")"

derive instance genericLispVal :: Generic LispVal _
derive instance eqLispVal :: Eq LispVal

instance showLispVal :: Show LispVal where
  show = showVal
  -- show (Atom a) = "(Atom " <> show a <> ")"
  -- show (List a) = "(List " <> show a <> ")"
  -- show (DottedList a _) = "(DottedList " <> show a <> ")"
  -- show (Number a) = "(Number " <> show a <> ")"
  -- show (String a) = "(String " <> show a <> ")"
  -- show (Bool a) = "(Bool " <> show a <> ")"
