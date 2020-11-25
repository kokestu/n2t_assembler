module Parser where

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
import Data.Map
  ( empty
  , lookup
  , Map
  , insertWith
  )
import Data.List (nub)

import Grammar

-- Use a Map for the symbol table
type Parser = Parsec String (Map String (Maybe Int))
type SymbolTable = Map String (Maybe Int)

parseProgram :: Parser ([Line], SymbolTable)
parseProgram = do
  lines <- many (spaces >> parseLine)
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
  if isJust x then checkIfExists symbol else return ()
  updateState $ insertWith update symbol x 
  return symbol
  where
    checkIfExists :: String -> Parser ()
    checkIfExists k = do
      symbols <- getState
      let val = Data.Map.lookup k symbols
      case val of
        Nothing -> return ()
        Just (Nothing) -> return ()
        Just _ -> fail "Label already exists in symbol table."

    update :: Maybe Int -> Maybe Int -> Maybe Int
    update (Just new) Nothing = Just new
    update Nothing old = old
    update _ _ = error "Should never happen"

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
cInstruction = (CAss <$> parseCAss)
              -- <|> JAss <$> parseJAss
              -- <|> parseJExpr)
              <* maybeCommentOrWhitespace

parseCAss :: Parser Assignment
parseCAss = do
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

lInstruction :: Parser LInstruction
lInstruction = Label <$> do
  line <- sourceLine <$> getPosition
  label <- between
    (char '(')
    (char ')')
    (parseSymbolAndUpdateTable $ Just line)
  maybeCommentOrWhitespace
  return label

comment :: Parser ()
comment = string "//" >> skipMany (noneOf "\n")

maybeCommentOrWhitespace :: Parser ()
maybeCommentOrWhitespace = spaces *> optional comment
