{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Accelerack.Interface where

import Foreign
import Foreign.C
import Foreign.Ptr

import Accelerack.Marshal
import Accelerack.Parse
import Control.Monad

foreign import ccall "dynamic" mkAllocFun :: FunPtr AllocFun -> AllocFun
foreign import ccall unsafe "acc.h modify_vector"
  modifyVector :: Ptr () -> CInt -> IO ()

type AllocFun = Int -> Ptr CInt -> Ptr () -> IO (Ptr ())

parse :: String -> IO Exp
parse = either fail return . (runWithKnown known . parseExp <=< lexSexp)

foreign export ccall print_array :: Ptr () -> IO ()
print_array p = do
  a <- peekAccArray p
  printAccArray a

{-
foreign export ccall run_accelerate :: Ptr () -> CString -> FunPtr AllocFun -> IO ()
run_accelerate dp sp fp = do
  str      <- peekCString sp
  v :: Segment <- peek $ castPtr dp
  undefined
  -- infer type, shape
  -- arr <- mkAllocFun fp shlen shp typp
-}

