module Parse where

import Prelude

import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Control.Monad.Error.Class (throwError)
import Data.Array (fold)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), many, (:))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Number as Number
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Error (ThrowsError)
import Error as Error
import LispVal (LispVal(..))
import Text.Parsing.Parser (Parser, failWithPosition, position, runParser)
import Text.Parsing.Parser.Combinators (endBy, sepBy, skipMany1, try)
import Text.Parsing.Parser.String (char, noneOf, oneOf, string)
import Text.Parsing.Parser.Token (digit, letter, space)

readExpr :: String -> ThrowsError LispVal
readExpr input = case runParser input parseExpr of
  Left err -> throwError $ Error.Parser err
  Right val -> pure val

parseExpr :: Parser String LispVal
parseExpr =  fix \_ ->
  parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do
        _ <- char '('
        x <- try parseList <|> parseDottedList
        _ <- char ')'
        pure x

parseString :: Parser String LispVal
parseString = do
  _ <- char '"'
  str <- many (escapeStrings <|> map singleton (noneOf $ toCharArray "\""))
  _ <- char '"'
  pure $ String $ fold str
  where
    escapeStrings :: Parser String String
    escapeStrings =
      string "\\\""
      <|> string "\n"
      <|> string "\t"
      <|> string "\\"

parseAtom :: Parser String LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = toStr $ first : rest
  pure $ case atom of
    "true" -> Bool true
    "false" -> Bool false
    _ -> Atom atom

parseNumber :: Parser String LispVal
parseNumber = do
  head <- digit
  tail <- many (digit <|> char '.')
  n <- getJustWrap $ Number.fromString $ toStr (head:tail)
  pure $ Number n


parseQuoted :: Parser String LispVal
parseQuoted = do
  _ <- char '\''
  x <- (parseExpr_ unit)
  pure $ List $ (Atom "quote") : x : Nil

parseList :: Parser String LispVal
parseList = fix \_ ->
  List <$> sepBy parseExpr spaces

parseDottedList :: Parser String LispVal
parseDottedList = fix \_ -> do
  head <- endBy parseExpr spaces
  tail <- char '.' *> spaces *> parseExpr
  pure $ DottedList head tail

parseExpr_ :: Unit -> Parser String LispVal
parseExpr_ _ = parseExpr
-- Helpers

symbol :: Parser String Char
symbol = oneOf $ toCharArray "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser String Unit
spaces = skipMany1 space

toStr :: forall f. Foldable f => f Char -> String
toStr = Array.fromFoldable >>> fromCharArray

-- Low Level Helpers
many1 :: forall a. Parser String a -> Parser String (NonEmptyList a)
many1 p = do
  a <- p
  as <- many p
  pure $ NonEmptyList $ NonEmpty a as

getJust :: forall a. Parser String (Maybe a) -> Parser String a
getJust p = do
  may <- p
  pos <- position
  case may of
    Nothing -> failWithPosition "Nothing in maybe. Expected Just" pos
    Just a -> pure a

getJustWrap :: forall a. Maybe a -> Parser String a
getJustWrap = getJust <<< pure
