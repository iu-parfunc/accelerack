{-# LANGUAGE LambdaCase #-}

module Main where

import Language.Haskell.Interpreter
import Accelerack.Parse
import Accelerack.Gen
import Accelerack.Run

main :: IO ()
main = do
  interp $ do
    setImportsQ [("Data.List",Just "L")]
    interpret "L.map (+1) [1..4]" (as :: [Int])
  interp $ do
    setImportsQ [("Data.List",Just "L")]
    eval "map (+2) [3..8]"

interp :: Show a => Interpreter a -> IO ()
interp m = do
  res <- runInterpreter m
  putStrLn $ replicate 20 '-'
  case res of
    Left err -> putStrLn $ "Interpreter Error: " ++ show err
    Right a  -> print a

