module Assemble (assemble) where

import Grammar
import qualified Parser.Types as PT
import Control.Monad.State
import Data.List (intercalate)

data AssembleState = AssembleState
  { symbolTable :: PT.SymbolTable
  , variableCount :: Int
  }

type Assembly = State AssembleState

assemble :: [Line] -> PT.ParserState -> String
assemble lines parserState = result
  where
    (result, _) = runState assembled initialState
    initialState = AssembleState
      { symbolTable = PT.symbolTable parserState
      , variableCount = 0
      }
    assembled = intercalate "\n" <$> mapM assembleLine lines

assembleLine :: Line -> Assembly String
assembleLine = return . show
