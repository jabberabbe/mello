{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{- |
Copyright: (c) 2021 Tito Sacchi
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tito Sacchi <tito.sakki@gmail.com>

GDB/MI (machine interface) output protocol parser.
The complete specification can be found in the GDB manual, section 27.2.2:
https://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Output-Syntax.html
-}

module Mello.GdbMI
       ( MIMessage(..)
       , parseGdbOutput
       , Value(..)
       , ResultRecord(..)
       , ResultClass(..)
       , AsyncRecord(..)
       , AsyncType(..)
       , StreamRecord(..)
       , StreamType(..)
       ) where

import           Control.Applicative        hiding (many, some)

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
  deriving stock (Show)

data ResultClass =
    Done
  | Connected
  | Error
  | Exit
  deriving stock (Show)

data ResultRecord = ResultRecord
  {
    resultToken   :: Maybe Int,
    resultClass   :: ResultClass,
    resultResults :: HashMap Text Value
  } deriving stock (Show)

data StreamRecord = StreamRecord StreamType Text deriving stock (Show)
data StreamType =
    Console
  | Target
  | Log
  deriving stock (Show)

data AsyncType =
    Status
  | Exec
  | Notify
  deriving stock (Show)

data AsyncRecord = AsyncRecord
  {
    asyncToken   :: Maybe Int,
    asyncType    :: AsyncType,
    asyncClass   :: Text,
    asyncResults :: HashMap Text Value
  } deriving stock (Show)

data MIMessage =
    Result ResultRecord
  | Async AsyncRecord
  | Stream StreamRecord
  deriving stock (Show)

parseGdbOutput :: Text -> Maybe [MIMessage]
parseGdbOutput = parseMaybe parseMIMessages

parseMIMessages :: Parser [MIMessage]
parseMIMessages = many parseMIMessage
  <* string "(gdb)" <* optional (char ' ') <* newline

parseMIMessage :: Parser MIMessage
parseMIMessage = choice
  [ Async  <$> parseAsyncRecord
  , Result <$> parseResultRecord
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
  <$> (optional L.decimal <* char '^')
  <*> parseResultClass
  <*> ((char ',' *> parseAttrs) <|> pure HM.empty)
  <*  newline

parseAsyncRecord :: Parser AsyncRecord
parseAsyncRecord = AsyncRecord
  <$> optional L.decimal
  <*> choice
    [char '*' *> pure Exec, char '+' *> pure Status, char '=' *> pure Notify]
  <*> parseIdent
  <*> ((char ',' *> parseAttrs) <|> pure HM.empty)
  <* newline

parseStreamRecord :: Parser StreamRecord
parseStreamRecord = StreamRecord
  <$> choice
    [char '~' *> pure Console, char '@' *> pure Target, char '&' *> pure Log]
  <*> parseString
  <* newline

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

