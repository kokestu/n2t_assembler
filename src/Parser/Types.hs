{-# LANGUAGE RecordWildCards #-}

module Parser.Types where

import Text.Parsec hiding (Line)
import Data.Map
  ( empty
  , lookup
  , Map
  , insert
  )

-- Use a Map for the symbol table
type Parser = Parsec String ParserState
type SymbolTable = Map String Int

data ParserState = ParserState
  { symbolTable :: SymbolTable
  , instructionCount :: Int
  } deriving (Show)

emptyState :: ParserState
emptyState = ParserState
  { symbolTable = empty
  , instructionCount = 0
  }

getInstructionCount :: Parser Int
getInstructionCount = instructionCount <$> getState

incInstructionCount :: Parser ()
incInstructionCount = do
  st@ParserState{..} <- getState
  putState $ st { instructionCount = instructionCount + 1 }

updateTable :: String -> Int -> Parser ()
updateTable symbol x = do
  st@ParserState{..} <- getState
  checkIfExists symbol symbolTable
  let symtab' = insert symbol x symbolTable
  putState $ st { symbolTable = symtab' }
  where
    checkIfExists k sym = case Data.Map.lookup k sym of
      Nothing -> return ()
      Just _ -> fail "Label already exists in symbol table."

