module Parser where

import Text.Parsec
  ( Parsec
  , (<|>)
  )
import Text.Parsec.Char
import Grammar

type Parser = Parsec String ()

parseLine :: Parser Line
parseLine = (Left <$> aInstruction)
         <|> (Right <$> cInstruction)
         <* maybeCommentOrWhitespace

maybeCommentOrWhitespace :: Parser ()
maybeCommentOrWhitespace = undefined

aInstruction :: Parser AInstruction
aInstruction = undefined

cInstruction :: Parser CInstruction
cInstruction = undefined
