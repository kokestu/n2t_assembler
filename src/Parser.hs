module Parser where

import Text.Parsec
  ( Parsec
  , (<|>)
  )
import Text.Parsec.Char
import Grammar

type Parser = Parsec String ()

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
