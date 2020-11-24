module Main where

import System.Environment   
import Data.List  

import Grammar
import Parser 

main :: IO ()
main = do  
  [src, target] <- getArgs
  contents <- readFile src
  writeFile target $ compile contents
  putStrLn "Done."

compile :: String -> String
compile = id
