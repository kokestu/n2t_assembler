module Parser (parseAndGetState) where

import Text.Parsec hiding (Line)
import Text.Parsec.Char
  ( char
  , newline
  , spaces
  , noneOf
  , string
  , letter
  , digit
  , oneOf
  )
import Data.Maybe
  ( catMaybes
  , isJust
  )
import Data.List (nub)
import Parser.Types

import Grammar

parseAndGetState :: String -> String -> Either ParseError ([Line], ParserState)
parseAndGetState = runParser parseProgram emptyState

parseProgram :: Parser ([Line], ParserState)
parseProgram = do
  lines <- many (spaces >> parseLine)
  st <- getState
  return (catMaybes lines, st)

parseLine :: Parser (Maybe Line)
parseLine = (Just . AIn <$> aInstruction <* incInstructionCount)
         <|> (Just . CIn <$> cInstruction <* incInstructionCount)
             -- only needed to update symbol table
         <|> (lInstruction *> pure Nothing)  
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
        <|> UDefSymbol <$> (try $ parseIdentifier)

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
cInstruction = try (CAss <$> parseAss)
              <|> try parseJAss
              <|> parseJExpr
              <* maybeCommentOrWhitespace

parseJAss :: Parser CInstruction
parseJAss = do
  assignment <- parseAss
  char ';'
  jump <- parseJump
  return (JAss assignment jump)

parseJump :: Parser Jump
parseJump = try (string "JMP" *> pure JMP)
            <|> try (string "JEQ" *> pure JEQ)
            <|> try (string "JNE" *> pure JNE)
            <|> try (string "JLT" *> pure JLT)
            <|> try (string "JLE" *> pure JLE)
            <|> try (string "JGT" *> pure JGT)
            <|> try (string "JGE" *> pure JGE)
            <|> try (string "null" *> pure JNull)

parseJExpr :: Parser CInstruction
parseJExpr = do
  expression <- parseExpr
  char ';'
  jump <- parseJump
  return (JExpr expression jump)

parseAss :: Parser Assignment
parseAss = do
  destination <- parseDest
  char '='
  expression <- parseExpr
  return (Ass destination expression)

parseDest :: Parser DestReg
parseDest = (do
  regs <- (try (count 3 parseReg)
          <|> try (count 2 parseReg)
          <|> pure <$> parseReg)
  if length (nub regs) /= length regs
  then fail "Duplicate destinations"
  else case regs of
    [x] -> return $ Single x
    [x,y] -> return $ Double x y
    [x,y,z] -> return $ Triple x y z
    _ -> error "PANIC: parsed too much"
  ) <|> (string "null" >> return RNull)

parseReg :: Parser Reg
parseReg = (char 'D' >> pure D)
         <|> (char 'M' >> pure M)
         <|> (char 'A' >> pure A)

parseExpr :: Parser Expr
parseExpr = try (opExpr '+' Add)
         <|> try (opExpr '-' Minus)
         <|> try (opExpr '&' And)
         <|> try (opExpr '|' Or)
         <|> (char '!' >> Not <$> constExpr)
         <|> C <$> constExpr

constExpr :: Parser ConstExpr
constExpr = char '0' *> pure Zero
          <|> char '1' *> pure One
          <|> Register <$> parseReg

opExpr :: Char -> (ConstExpr -> ConstExpr -> Expr) -> Parser Expr
opExpr op cons = do
  expr1 <- constExpr
  char op
  expr2 <- constExpr
  return $ cons expr1 expr2

lInstruction :: Parser ()
lInstruction = do
  line <- sourceLine <$> getPosition
  label <- between
    (char '(')
    (char ')')
    (do
      label <- parseIdentifier
      count <- getInstructionCount 
      updateTable label count
      return label)
  maybeCommentOrWhitespace

comment :: Parser ()
comment = string "//" >> skipMany (noneOf "\n")

maybeCommentOrWhitespace :: Parser ()
maybeCommentOrWhitespace = spaces *> optional comment
