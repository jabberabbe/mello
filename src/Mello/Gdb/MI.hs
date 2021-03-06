{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{- |
Copyright: (c) 2021 Tito Sacchi
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tito Sacchi <tito.sakki@gmail.com>

GDB/MI (machine interface) output protocol parser.
The complete specification can be found in the GDB manual, section 27.2.2:
https:\/\/sourceware.org\/gdb\/current\/onlinedocs\/gdb\/GDB_002fMI-Output-Syntax.html
-}

module Mello.Gdb.MI
       ( MIMessage(..)
       , parseGdbOutput
       , parseGdbOutputLine
       , Value(..)
       , valueToString
       , valueToTuple
       , valueToList
       , ResultRecord(..)
       , ResultClass(..)
       , AsyncRecord(..)
       , AsyncType(..)
       , StreamRecord(..)
       , StreamType(..)
       ) where

import           Control.Applicative        hiding (many, some)

import           Control.Arrow              (left)
import           Data.Char
import           Data.HashMap.Strict        as HM
import           Data.Text                  as T
import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

data Value =
    String Text
  | Tuple (HashMap Text Value)
  | List [(Maybe Text, Value)]
  deriving stock (Show, Eq)

-- Convenience functions, useful in the Maybe monad.
valueToString :: Value -> Maybe Text
valueToString (String x) = Just x
valueToString _          = Nothing

valueToTuple :: Value -> Maybe (HashMap Text Value)
valueToTuple (Tuple x) = Just x
valueToTuple _         = Nothing

valueToList :: Value -> Maybe [(Maybe Text, Value)]
valueToList (List x) = Just x
valueToList _        = Nothing

data ResultClass =
    Done
  | Connected
  | Error
  | Exit
  deriving stock (Show, Eq)

data ResultRecord = ResultRecord
  {
    resultToken   :: Maybe Int,
    resultClass   :: ResultClass,
    resultResults :: HashMap Text Value
  } deriving stock (Show, Eq)

data StreamRecord = StreamRecord StreamType Text deriving stock (Show, Eq)
data StreamType =
    Console
  | Target
  | Log
  deriving stock (Show, Eq)

data AsyncType =
    Status
  | Exec
  | Notify
  deriving stock (Show, Eq)

data AsyncRecord = AsyncRecord
  {
    asyncToken   :: Maybe Int,
    asyncType    :: AsyncType,
    asyncClass   :: Text,
    asyncResults :: HashMap Text Value
  } deriving stock (Show, Eq)

data MIMessage =
    Result ResultRecord
  | Async AsyncRecord
  | Stream StreamRecord
  deriving stock (Show, Eq)

parseGdbOutput :: Text -> Maybe [MIMessage]
parseGdbOutput = parseMaybe parseMIMessages

parseGdbOutputLine :: Text -> Either String (Maybe MIMessage)
parseGdbOutputLine = left errorBundlePretty . parse
   (pure Nothing <* string "(gdb)" <* optional (char ' ')
  <|> Just <$> parseMIMessage) ""

parseMIMessages :: Parser [MIMessage]
parseMIMessages = many (parseMIMessage <* eol)
  <* string "(gdb)" <* optional (char ' ') <* eol

parseMIMessage :: Parser MIMessage
parseMIMessage = choice
  [ Result <$> parseResultRecord
  , Async  <$> parseAsyncRecord
  , Stream <$> parseStreamRecord
  ]

parseResultClass :: Parser ResultClass
parseResultClass = choice
  [ string "done"         *> pure Done
    -- According to the GDB manual, "done" and "running"
    -- should be treated identically
  , string "running"      *> pure Done
  , string "connected"    *> pure Connected
  , string "error"        *> pure Error
  , string "exit"         *> pure Exit
  ]

parseResultRecord :: Parser ResultRecord
parseResultRecord = ResultRecord
  <$> try (optional L.decimal <* char '^')
  <*> parseResultClass
  <*> ((char ',' *> parseAttrs) <|> pure HM.empty)

parseAsyncRecord :: Parser AsyncRecord
parseAsyncRecord = AsyncRecord
  <$> optional L.decimal
  <*> choice
    [char '*' *> pure Exec, char '+' *> pure Status, char '=' *> pure Notify]
  <*> parseIdent
  <*> ((char ',' *> parseAttrs) <|> pure HM.empty)

parseStreamRecord :: Parser StreamRecord
parseStreamRecord = StreamRecord
  <$> choice
    [char '~' *> pure Console, char '@' *> pure Target, char '&' *> pure Log]
  <*> parseString

parseAttrs :: Parser (HashMap Text Value)
parseAttrs = fromList <$> parseTupleField `sepBy` (char ',')

parseTupleField :: Parser (Text, Value)
parseTupleField = (,)
  <$> parseIdent
  <*  char '='
  <*> parseValue

parseIdent :: Parser Text
parseIdent =
  takeWhile1P (Just "alphanumeric character, _ or -") isValidIdentChar

isValidIdentChar :: Char -> Bool
isValidIdentChar x = isAlphaNum x || x == '_' || x == '-'

parseValue :: Parser Value
parseValue = choice
  [ String <$> parseString
  , Tuple  <$> parseTuple
  , List   <$> parseList
  ]

parseTuple :: Parser (HashMap Text Value)
parseTuple = between (char '{') (char '}') parseAttrs

parseString :: Parser Text
parseString = char '\"' *> (T.pack <$> manyTill parseChar (char '\"'))

-- Apparently L.charLiteral does not recognize '\e'
parseChar :: Parser Char
parseChar = try L.charLiteral <|> try (string "\\e" *> pure '\x1B')

parseList :: Parser [(Maybe Text, Value)]
parseList = between (char '[') (char ']') $ parseListElem `sepBy` (char ',')

parseListElem :: Parser (Maybe Text, Value)
parseListElem =
      (\(a,b) -> (Just a, b)) <$> parseTupleField
  <|> (Nothing,)              <$> parseValue

