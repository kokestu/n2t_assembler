module Assemble (assemble) where

import Grammar
import qualified Parser.Types as PT
import Control.Monad.Trans.State
import Data.List (intercalate)
import qualified Data.Map as Map
import Text.Printf (printf)

data AssembleState = AssembleState
  { symbolTable :: PT.SymbolTable
  , variableCount :: Int
  }

modifySymTab :: (PT.SymbolTable -> PT.SymbolTable) -> Assembly () 
modifySymTab f = modify $ \as -> as { symbolTable = f $ symbolTable as }

modifyVarCount :: (Int -> Int) -> Assembly () 
modifyVarCount f = modify $ \as -> as { variableCount = f $ variableCount as }

type AssemblyError = String
type Assembly a = StateT AssembleState (Either AssemblyError) a

assemble :: [Line] -> PT.ParserState -> Either AssemblyError String
assemble lines parserState = result
  where
    result = evalStateT assembled initialState
    initialState = AssembleState
      { symbolTable = PT.symbolTable parserState
      , variableCount = 0
      }
    assembled = intercalate "\n" <$> mapM assembleLine lines

assembleLine :: Line -> Assembly String
assembleLine (AIn instr) = case instr of
  AtInt i ->
    if fromIntegral i >= 2**16
    then fail (show i ++ " is too big!")
    else return $ printf "0%015b" i
  AtSymbol sym -> 
    case sym of
      VR (R i) -> 
        if i >= 16
        then fail ("R" ++ show i ++ " doesn't exist!")
        else return $ printf "0%015b" i
      PP SP  -> return $ printf "0%015b" (0 :: Int)
      PP LCL -> return $ printf "0%015b" (1 :: Int)
      PP ARG -> return $ printf "0%015b" (2 :: Int)
      PP THIS -> return $ printf "0%015b" (3 :: Int)
      PP THAT -> return $ printf "0%015b" (4 :: Int)
      IP SCREEN -> return $ printf "0%015b" (16384 :: Int)
      IP KBD -> return $ printf "0%015b" (24576 :: Int)
      UDefSymbol sym -> do 
          addr <- getSymbolAddress sym
          return $ printf "0%015b" addr

assembleLine (CIn instr) = undefined


getSymbolAddress :: String -> Assembly Int
getSymbolAddress sym = do
    st <- gets symbolTable
    case Map.lookup sym st of
      Just address -> return address
      Nothing -> do
        vc <- gets variableCount
        let address = vc + 16
        modifySymTab (Map.insert sym address)
        modifyVarCount (+1)
        return address
