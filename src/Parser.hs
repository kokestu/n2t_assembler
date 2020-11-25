module Parser where

import Text.Parsec hiding (Line)
import Text.Parsec.Char
  ( char
  , newline
  , spaces
  , anyChar
  , string
  , letter
  , digit
  , oneOf
  )
import Data.Maybe (catMaybes)
import Data.Map

import Grammar

-- Use a Map for the symbol table
type Parser = Parsec String (Map String (Maybe Int))
type SymbolTable = Map String (Maybe Int)

parseProgram :: Parser ([Line], SymbolTable)
parseProgram = do
  lines <- many parseLine
  st <- getState
  return (catMaybes lines, st)

parseLine :: Parser (Maybe Line)
parseLine = (Just . AIn <$> aInstruction)
         <|> (Just . CIn <$> cInstruction)
         <|> (Just . LIn <$> lInstruction)
         <|> (maybeCommentOrWhitespace *> pure Nothing)
         <* newline

aInstruction :: Parser AInstruction
aInstruction = char '@' >> (parseInt <|> parseSymbol)  <* maybeCommentOrWhitespace
  where
    parseInt = AtInt <$> (read <$> many1 digit)
    parseSymbol = AtSymbol <$> symbol

symbol :: Parser Symbol
symbol = VR <$> (try parseVR)
        <|> PP <$> (try parsePP)
        <|> IP <$> (try parseIP)  
        <|> UDefSymbol <$> (try $ parseSymbolAndUpdateTable Nothing)

parseSymbolAndUpdateTable :: Maybe Int -> Parser String
parseSymbolAndUpdateTable x = do
  symbol <- parseIdentifier
  updateState $ insertWith (flip const) symbol x 
  return symbol

parseVR :: Parser VirtualRegister
parseVR = char 'R' >> R <$> (read <$> many1 digit)

parsePP :: Parser PredefinedPointer
parsePP = (string "SP" *> pure SP)
          <|> (string "LCL" *> pure LCL)
          <|> (string "ARG" *> pure ARG)
          <|> (string "THIS" *> pure THIS)
          <|> (string "THAT" *> pure THAT)
          

parseIP :: Parser IOPointer
parseIP = (string "SCREEN" *> pure SCREEN)
        <|> (string "KBD" *> pure KBD)

parseIdentifier :: Parser String
parseIdentifier = do
  firstChar <- nonDigitChar
  rest <- maybeDigitCharList
  return (firstChar:rest)
  where
    nonDigitChar = oneOf "_.$:" <|> letter
    maybeDigitCharList = many (nonDigitChar <|> digit)

cInstruction :: Parser CInstruction
cInstruction = string "this should never work!!1!"
    >> (return $ CAss (Ass (Single D) (C Zero)))

lInstruction :: Parser LInstruction
lInstruction = Label <$> do
  line <- Just <$> sourceLine <$> getPosition
  label <- between
    (char '(')
    (char ')')
    (parseSymbolAndUpdateTable line)
  maybeCommentOrWhitespace
  return label

comment :: Parser ()
comment = string "//" >> skipMany anyChar

maybeCommentOrWhitespace :: Parser ()
maybeCommentOrWhitespace = spaces *> optional comment
