module Main where

import System.Environment   
import Data.List  

import Grammar
import Parser 
import Assemble

main :: IO ()
main = do  
  [src, target] <- getArgs
  contents <- readFile src
  let maybeParsed = parseAndGetState src contents
  case maybeParsed of
    Left err -> print err
    Right (parsed, state) -> do
      let bin = assemble parsed state
      writeFile target bin
      putStrLn "Done."

