{-# Language TypeOperators #-}

module Main where

import Data.Array.Accelerate
import System.Eval

imports :: [Import]
imports =
  [ "qualified Data.Array.Accelerate.Interpreter as I"
  , "qualified Data.Array.Accelerate as A"
  ]


prog1 = "show (10:: A.Exp Int) :: String"

prog2 = "I.run (A.generate (A.index1 (10:: A.Exp Int)) A.unindex1) :: (A.Array A.DIM1 Int)"

prog3 = "I.run $ A.map (+1) (A.use (A.fromList (A.Z A.:.3 A.:.5) [1..] :: A.Array A.DIM2 Int))"

prog4 = "show (I.run $ (A.use (A.fromList (A.Z A.:.3 A.:.5) [1..] :: A.Array A.DIM2 Int)))"

main = do 
  --x <- eval "33+22 ::Int" imports 
  --print (x::Maybe Int)

  --x <- eval prog1 imports 
  --print (x:: Maybe String)

  --x <- eval prog2 imports 
  --print (x:: Maybe (Array (Z :. Int) Int))

  x <- eval prog4 imports 
  print (x:: Maybe String) --(Array (Z :. Int :. Int) Int))
