{-# LANGUAGE LambdaCase #-}


module Main where

import Language.Haskell.Interpreter as H
import Accelerack.Parse
import Accelerack.Gen
import Accelerack.Run

main :: IO ()
main = do test1; test2; test3

test1 =
  interp $ do
    setImportsQ [("Data.List",Just "L")]
    interpret "L.map (+1) [1..4]" (as :: [Int])

test2 = 
  interp $ do
    setImportsQ [("Data.List",Just "L")]
    eval "map (+2) [3..8]"

test3 = interp $ 
  do setImports ["Prelude", "Data.Array.Accelerate", "Data.Array.Accelerate.Interpreter"] 
     eval "run $ generate (index1 10) (\\ x -> x) :: (Vector ((:.) Z Int))"

-- Should print:
-- Right "Array (Z :. 10) [Z :. 0,Z :. 1,Z :. 2,Z :. 3,Z :. 4,Z :. 5,Z :. 6,Z :. 7,Z :. 8,Z :. 9]"

----------------------------------------------------------------------------------

interp :: Show a => Interpreter a -> IO ()
interp m = do
  res <- runInterpreter m
  putStrLn $ replicate 20 '-'
  case res of
    Left err -> putStrLn $ "Interpreter Error: " ++ show err
    Right a  -> print a

