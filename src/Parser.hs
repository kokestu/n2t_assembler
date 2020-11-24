module Parser where

import Text.Parsec
  ( Parsec
  , (<|>)
  , many
  , many1
  , between
  , skipMany
  , optional
  )
import Text.Parsec.Char
  ( char
  , newline
  , spaces
  , anyChar
  , string
  , letter
  )
import Data.Maybe (catMaybes)

import Grammar

type Parser = Parsec String ()

parseProgram :: Parser [Line]
parseProgram = catMaybes <$> many parseLine

parseLine :: Parser (Maybe Line)
parseLine = (Just . AIn <$> aInstruction)
         <|> (Just . CIn <$> cInstruction)
         <|> (Just . LIn <$> lInstruction)
         <|> (maybeCommentOrWhitespace *> pure Nothing)
         <* newline

aInstruction :: Parser AInstruction
aInstruction = undefined

cInstruction :: Parser CInstruction
cInstruction = undefined

lInstruction :: Parser LInstruction
lInstruction = Label <$> between
  (char '(')
  (char ')')
  (many1 letter)
  <* maybeCommentOrWhitespace

comment :: Parser ()
comment = string "//" >> skipMany anyChar

maybeCommentOrWhitespace :: Parser ()
maybeCommentOrWhitespace = spaces *> optional comment
