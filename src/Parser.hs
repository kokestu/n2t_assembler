module Parser where

import Text.Parsec
  ( Parsec
  , (<|>)
  , many
  )
import Text.Parsec.Char
import Data.Maybe (catMaybes)

import Grammar

type Parser = Parsec String ()

parseProgram :: Parser [Line]
parseProgram = catMaybes <$> many parseLine

parseLine :: Parser (Maybe Line)
parseLine = (Just . Left <$> aInstruction)
         <|> (Just . Right <$> cInstruction)
         <|> (maybeCommentOrWhitespace *> pure Nothing)

maybeCommentOrWhitespace :: Parser ()
maybeCommentOrWhitespace = undefined

aInstruction :: Parser AInstruction
aInstruction = undefined

cInstruction :: Parser CInstruction
cInstruction = undefined
