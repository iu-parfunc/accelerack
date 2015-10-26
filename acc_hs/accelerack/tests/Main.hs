{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.TH
import Data.List

prop_1 (xs :: [Int]) = sort xs == sort (reverse xs)

prop_2 (x :: Int) = (x^7 - x) `mod` 7 == 0

case_1 = compare l1 l2 @?= GT
  where
  l1, l2 :: [Int]
  l1 = [1,2,3]
  l2 = [1,2]

main :: IO ()
main = $(defaultMainGenerator)

-- test1 = interp $ do
--   setImportsQ [("Data.List",Just "L")]
--   interpret "L.map (+1) [1..4]" (as :: [Int])
-- 
-- test2 = interp $ do
--   setImportsQ [("Data.List",Just "L")]
--   eval "map (+2) [3..8]"
-- 
-- test3 = interp $ do 
--   setImports ["Prelude", "Data.Array.Accelerate", "Data.Array.Accelerate.Interpreter"] 
--   eval "run $ generate (index1 10) (\\ x -> x) :: (Vector ((:.) Z Int))"
-- 
-- test4 = interp $ do
--   getModuleExports "Data.Array.Accelerate"
-- 
-- -- Should print:
-- -- Right "Array (Z :. 10) [Z :. 0,Z :. 1,Z :. 2,Z :. 3,Z :. 4,Z :. 5,Z :. 6,Z :. 7,Z :. 8,Z :. 9]"
-- 
-- ----------------------------------------------------------------------------------
-- 
-- interp :: Show a => Interpreter a -> IO ()
-- interp m = do
--   res <- runInterpreter m
--   putStrLn $ replicate 20 '-'
--   case res of
--     Left err -> putStrLn $ "Interpreter Error: " ++ show err
--     Right a  -> print a

