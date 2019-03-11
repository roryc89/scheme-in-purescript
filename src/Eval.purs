module Eval where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either)
import Data.List (List(..), (:))
import Data.Maybe (maybe)
import Data.NonEmpty (NonEmpty(..), foldl1)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), lookup)
import Error (LispError(..), ThrowsError)
import LispVal (LispVal(..))

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = pure val
eval val@(Number _) = pure val
eval val@(Bool _) = pure val
eval (List (Atom "quote" : val : Nil)) = pure val
eval (List (Atom "if" : pred : conseq : alt : Nil)) =
     do
       result <- eval pred
       case result of
         Bool false -> eval alt
         otherwise  -> eval conseq

eval (List (Atom func : args)) = traverse eval args >>= applyLisp func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

applyLisp :: String -> List LispVal -> ThrowsError LispVal
applyLisp func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ((#) args)
    (lookup func primitives)

primitives :: Array (Tuple String (List LispVal -> ThrowsError LispVal))
primitives =
  [ Tuple "+" (numericBinop (+))
  , Tuple "-" (numericBinop (-))
  , Tuple "*" (numericBinop (*))
  , Tuple "/" (numericBinop div)
  , Tuple "mod" (numericBinop mod)
  , Tuple "=" (numBoolBinop (==))
  , Tuple "<" (numBoolBinop (<))
  , Tuple ">" (numBoolBinop (>))
  , Tuple "/=" (numBoolBinop (/=))
  , Tuple ">=" (numBoolBinop (>=))
  , Tuple "<=" (numBoolBinop (<=))
  , Tuple "&&" (boolBoolBinop (&&))
  , Tuple "||" (boolBoolBinop (||))
  , Tuple "string=?" (strBoolBinop (==))
  , Tuple "string<?" (strBoolBinop (<))
  , Tuple "string>?" (strBoolBinop (>))
  , Tuple "string<=?" (strBoolBinop (<=))
  , Tuple "string>=?" (strBoolBinop (>=))
  ]

numericBinop :: (Number -> Number -> Number) -> List LispVal -> ThrowsError LispVal
numericBinop op params =
  case params of
    Nil ->
      throwError $ NumArgs 2 Nil

    val@(a:Nil) ->
      throwError $ NumArgs 2 val

    (head:tail) ->
      traverse unpackNum (NonEmpty head tail)
        >>= (foldl1 op >>> Number >>> pure)

boolBinop :: forall a. (LispVal -> ThrowsError a) -> (a -> a -> Boolean) -> List LispVal -> ThrowsError LispVal
boolBinop unpacker op args =
  case args of
    (h1:h2:Nil) -> do
      left <- unpacker h1
      right <- unpacker h2
      pure $ Bool $ left `op` right

    _ ->
      throwError $ NumArgs 2 args

numBoolBinop :: (Number -> Number -> Boolean) -> List LispVal -> Either LispError LispVal
numBoolBinop  = boolBinop unpackNum
strBoolBinop :: (String -> String -> Boolean) -> List LispVal -> Either LispError LispVal
strBoolBinop  = boolBinop unpackStr
boolBoolBinop :: (Boolean -> Boolean -> Boolean) -> List LispVal -> Either LispError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Number
unpackNum (Number n) = pure n
unpackNum (List (n:Nil)) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = pure s
unpackStr (Number s) = pure $ show s
unpackStr (Bool s)   = pure $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Boolean
unpackBool (Bool b) = pure b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool
