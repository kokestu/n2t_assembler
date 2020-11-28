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

assembleLine (CIn instr) = case instr of
  CAss (Ass destination expression) -> do
    assembledExpr <- assembleExpr expression
    assembledDest <- assembleDestination destination
    return $ printf "111%s%s000" assembledExpr assembledDest
  JAss _ _ -> undefined
  JExpr _ _ -> undefined

assembleDestination :: DestReg -> Assembly String
assembleDestination dest
  | Triple _ _ _ <- dest = return "111"
  | Double A D <- dest   = return "110"
  | Double A M <- dest   = return "101"
  | Double M D <- dest   = return "011"
  | Double x y <- dest
  , x == y               = fail "PANIC! Double of the same register"
  | Double x y <- dest   = assembleDestination $ Double y x
  | Single A <- dest     = return "100"
  | Single M <- dest     = return "010"
  | Single D <- dest     = return "001"
  | RNull  <- dest       = return "000"

assembleExpr :: Expr -> Assembly String
assembleExpr expr
  | Zero <- expr = return "0101010"
  | C One <- expr = return "0111111"
  | C (Register A) <- expr = return "011000" 
  | C (Register M) <- expr = return "111000" 
  | C (Register D) <- expr = return "001100" 
  | Add (Register A) One <- expr = return "0110111"
  | Add (Register A) (Register D) <- expr = return "0110111"
  | Add (Register D) One <- expr = return "0011111"
  | Add x y <- expr
  , x == y = fail "Must add different things and also not A+M" 
  | Add (Register M) x <- expr = ('1':) . (drop 1) <$> assembleExpr (Add (Register A) x)
  | Add x y <- expr = assembleExpr $ Add y x
  | Minus (Register A) One <- expr = return "0110010"
  | Minus (Register A) (Register D) <- expr = return "0000111"
  | Minus (Register D) (Register A) <- expr = return "0010011"
  | Minus (Register D) One <- expr = return "0001110"
  | Minus One _ <- expr = fail "1 - anything is not allowed"
  | Minus x y <- expr
  , x == y = fail "Must add different things and also not A+M" 
  | Minus (Register M) x <- expr = ('1':) . (drop 1)  <$> assembleExpr (Minus (Register A) x)
  | Minus x (Register M) <- expr = ('1':) . (drop 1)  <$> assembleExpr (Minus x (Register A))
  | And (Register D) (Register A) <- expr = return "0000000"
  | And (Register A) (Register D) <- expr = return "0000000"
  | And (Register A) (Register M) <- expr = return "1000000"
  | And (Register M) (Register A) <- expr = return "1000000"
  | And _ _ <- expr = fail "And is only allowed between D and either A or M"
  | Or (Register D) (Register A) <- expr = return "0010101"
  | Or (Register A) (Register D) <- expr = return "0010101"
  | Or (Register A) (Register M) <- expr = return "1010101"
  | Or (Register M) (Register A) <- expr = return "1010101"
  | Or _ _ <- expr = fail "Or is only allowed between D and either A or M"
  | Negate (Register A) <- expr = return "0110011"
  | Negate (Register M) <- expr = return "1110011"
  | Negate (Register D) <- expr = return "0001111" 
  | Negate _ <- expr = fail "-1 is not a valid expression"
  | Not (Register A) <- expr = return "0110001"
  | Not (Register M) <- expr = return "1110001"
  | Not (Register D) <- expr = return "0001101" 
  | Not _ <- expr = fail "Not can't be applied to 1"

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
