{-# Language TypeOperators #-}

module Main where

import Data.Array.Accelerate
import System.Eval

imports :: [Import]
imports =
  [ "qualified Data.Array.Accelerate.Interpreter as I"
  , "qualified Data.Array.Accelerate as A"
  ]

prog = "I.run (A.generate (A.index1 (10:: A.Exp Int)) A.unindex1) :: (A.Array A.DIM1 Int)"

main = do 
  x <- eval "33::Int" imports 
  print (x::Maybe Int)

  x <- eval prog imports 
  print (x:: Maybe (Array (Z :. Int) Int))
