module Eval where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), all, drop, last, length, zip, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty(..), foldl1)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import LispVal (EffThrowsError, Env, LispError(..), LispVal(..), ThrowsError, bindVars, defineVar, getVar, liftThrows, nullEnv, setVar, showEnv, showVal)
--
eval :: Env -> LispVal -> EffThrowsError LispVal
eval env = case _ of
  val@(String _) -> pure val
  val@(Number _) -> pure val
  val@(Bool _) -> pure val
  (Atom id) -> getVar env id
  (List (Atom "quote" : val : mempty)) -> pure val

  (List (Atom "if" : pred : conseq : alt : mempty)) ->
     do
       result <- eval env pred
       case result of
         Bool true  -> eval env conseq
         Bool false -> eval env alt
         _ -> throwError $ TypeMismatch "Boolean" result

  (List (Atom "showEnv" : mempty)) -> do
     env_ <- liftEffect $ Ref.read env
     str_ <- liftEffect $ showEnv env_
     log str_
     pure (List mempty)

  (List (Atom "set!" : Atom var : form : mempty)) ->
     eval env form >>= setVar env var

  (List (Atom "define" : Atom var : form : mempty)) ->
     eval env form >>= defineVar env var

  (List (Atom "define" : List (Atom var : params) : body)) ->
     makeNormalFunc env params body >>= defineVar env var

  (List (Atom "define" : DottedList (Atom var : params) varargs : body)) ->
     makeVarArgs varargs env params body >>= defineVar env var

  (List (Atom "lambda" : List params : body)) ->
     makeNormalFunc env params body

  (List (Atom "lambda" : DottedList params varargs : body)) ->
     makeVarArgs varargs env params body

  (List (Atom "lambda" : varargs@(Atom _) : body)) ->
     makeVarArgs varargs env mempty body

  (List (function : args)) -> do
     func <- eval env function
     argVals <- traverse (eval env) args
     applyLisp func argVals

  badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm
  where
    makeFunc vararg closure params body = pure $ Func {params:(map showVal params), vararg, body, closure}
    makeNormalFunc = makeFunc Nothing
    makeVarArgs = makeFunc <<< Just <<< showVal


applyLisp :: LispVal -> List LispVal -> EffThrowsError LispVal
applyLisp (PrimitiveFunc func) args = liftThrows $ func args

applyLisp (Func {params, vararg, body, closure}) args =

  if num params /= num args && vararg == Nothing then
    throwError $ NumArgs (num params) args
  else
    ((zip >>> map Array.fromFoldable) params args
    # bindVars closure
    # liftEffect
    )
    >>= bindVarArgs vararg
    >>= evalBody

  where
    remainingArgs = drop (length params) args
    num = length
    evalBody env = map (last >>> fromMaybe (List mempty)) $ traverse (eval env) body
    bindVarArgs arg env = case arg of
        Just argName -> liftEffect $ bindVars env [ Tuple argName (List $ remainingArgs)]
        Nothing -> pure env

applyLisp func args = liftThrows $
  -- maybe
    (throwError $ NotFunction "Unrecognized primitive function args" (show func))
    -- ((#) args)
    -- (lookup func primitives)
primitiveBindings :: Effect Env
primitiveBindings =
  nullEnv
  >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc = map PrimitiveFunc



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
  , Tuple "car" car
  , Tuple "cdr" cdr
  , Tuple "cons" cons
  , Tuple "eq?" eqv
  , Tuple "eqv?" eqv
  , Tuple "equal?" equal
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

-- PRIMITIVE FUNCTIONS

car :: List LispVal -> ThrowsError LispVal
car ((List (x : xs)) : Nil) =
  pure x
car ((DottedList (x : xs) _) : Nil) =
  pure x
car (badArg : Nil) =
  throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: List LispVal -> ThrowsError LispVal
cdr ((List (x : xs)) : Nil) = pure $ List xs
cdr ((DottedList (_ : Nil) x) : Nil) =  pure x
cdr ((DottedList (_ : xs) x) : Nil) = pure $ DottedList xs x
cdr (badArg : Nil) =
  throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList


cons :: List LispVal -> ThrowsError LispVal
cons (x1 : List Nil : Nil) = pure $ List (x1 : Nil)
cons (x : List xs : Nil) = pure $ List $ x : xs
cons (x : DottedList xs xlast : Nil) = pure $ DottedList (x : xs) xlast
cons (x1 : x2 : Nil) = pure $ DottedList (x1 : Nil) x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: List LispVal -> ThrowsError LispVal
eqv ((Bool arg1) : (Bool arg2) : Nil) =
  pure $ Bool $ arg1 == arg2

eqv ((Number arg1) : (Number arg2) : Nil) =
  pure $ Bool $ arg1 == arg2

eqv ((String arg1) : (String arg2) : Nil) =
  pure $ Bool $ arg1 == arg2

eqv ((Atom arg1) : (Atom arg2) : Nil) =
  pure $ Bool $ arg1 == arg2

eqv ((DottedList xs x) : (DottedList ys y) : Nil) =
  eqv $
    List (xs <> (pure x))
    : (List (ys <> (pure y)))
    : Nil

eqv ((List arg1) : (List arg2) : Nil) =
  let
    eqvPair (Tuple x1 x2) = case eqv (x1 : x2 : Nil) of
      Left err -> false
      Right (Bool val) -> val
      Right _ -> false
  in
  pure $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)

eqv (_ : _ : Nil)                          = pure $ Bool false
eqv badArgList                             = throwError $ NumArgs 2 badArgList


unpackEquals :: forall a. Eq a => LispVal -> LispVal -> (LispVal -> ThrowsError a) -> ThrowsError Boolean
unpackEquals arg1 arg2 unpacker = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  pure $ unpacked1 == unpacked2
  `catchError` (const $ pure false)


equal :: List LispVal -> ThrowsError LispVal
equal (arg1 : arg2 : Nil) = do
  primitiveEquals <-
    (unpackEquals arg1 arg2 unpackNum)
    <|> (unpackEquals arg1 arg2 unpackStr)
    <|> (unpackEquals arg1 arg2 unpackBool)

  eqvEquals <- eqv (arg1 : arg2 : Nil)

  pure $ case eqvEquals of
    Bool b -> Bool $ (primitiveEquals || b)
    _ -> Bool primitiveEquals

equal badArgList = throwError $ NumArgs 2 badArgList
