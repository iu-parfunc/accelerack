{-# Language TypeOperators #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# NOINLINE foo #-}

module SxpInterface where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Marshal.Array

import Control.Applicative
import Control.Monad
import Control.Monad.State

import qualified Data.Array.Accelerate as A -- hiding ((++), replicate, product)
import Data.Array.Accelerate.IO (fromPtr, toPtr)
import Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Array.Sugar as Sugar

import Data.Proxy (Proxy(..))
import Data.IORef
import qualified Data.List as L
import Text.Read (readMaybe)

import qualified SxpParse as Parse
import ExUtils (Segment(..),AccArray(..))
import qualified ExUtils as Ex

import Language.Haskell.TH
import Language.Haskell.TH.Ppr
import Language.Haskell.Interpreter
import Data.String.Utils as Ds
import Data.Either.Unwrap
import Data.List.Split
import System.IO.Unsafe

-- Exporting defined functions
foreign export ccall entrypoint :: Int -> IO Int
foreign export ccall runAcc :: CString -> Ptr AccArray -> Ptr AccArray -> IO ()

-- Racket entry point function
entrypoint :: Int -> IO Int
entrypoint x = do
  print "Hello from Haskell"
  return x

getReferenceStr :: Int -> Ptr AccArray -> IO String
getReferenceStr refNum arr = do
  let ptrSize = 8 :: Int
  AccArray atyp ashp adata <- peek arr
  Segment s1 t1 dsg <- peek adata
  adata2 <- peekByteOff dsg (refNum * ptrSize)
  AccArray atyp' ashp' adata3 <- peek adata2
  -- Segment s1' t1' dsg'' <- peek adata3
  -- AccArray atyp' ashp' adata' <- peek dsg
  segdata <- Ex.peekSegmentPtrs adata3
  Segment s1ls' s1ts' s1da' <- peek adata3
  Segment s1ls s1ts s1da <- peek ashp'
  sh1 <- peekArray (Prelude.fromIntegral s1ls) (castPtr s1da :: Ptr CInt)
  let arrayStrFn = case Prelude.length sh1 of
        0 -> let psh = (Proxy :: Proxy DIM0)
          in case s1ts' of
            0 -> arrayReferenceStr psh (Proxy :: Proxy Int32)
            1 -> arrayReferenceStr psh (Proxy :: Proxy Double)
        1 -> let psh = (Proxy :: Proxy DIM1)
          in case s1ts' of
            0 -> arrayReferenceStr psh (Proxy :: Proxy Int32)
            1 -> arrayReferenceStr psh (Proxy :: Proxy Double)
        2 -> let psh = (Proxy :: Proxy DIM2)
          in case s1ts' of
            0 -> arrayReferenceStr psh (Proxy :: Proxy Int32)
            1 -> arrayReferenceStr psh (Proxy :: Proxy Double)
        3 -> let psh = (Proxy :: Proxy DIM3)
          in case s1ts' of
            0 -> arrayReferenceStr psh (Proxy :: Proxy Int32)
            1 -> arrayReferenceStr psh (Proxy :: Proxy Double)
        4 -> let psh = (Proxy :: Proxy DIM4)
          in case s1ts' of
            0 -> arrayReferenceStr psh (Proxy :: Proxy Int32)
            1 -> arrayReferenceStr psh (Proxy :: Proxy Double)
        5 -> let psh = (Proxy :: Proxy DIM5)
          in case s1ts' of
            0 -> arrayReferenceStr psh (Proxy :: Proxy Int32)
            1 -> arrayReferenceStr psh (Proxy :: Proxy Double)
        6 -> let psh = (Proxy :: Proxy DIM6)
          in case s1ts' of
            0 -> arrayReferenceStr psh (Proxy :: Proxy Int32)
            1 -> arrayReferenceStr psh (Proxy :: Proxy Double)
  arrayStrFn sh1 segdata

arrayReferenceStr :: (Ex.AccelerackShape sh, Ex.AccelerackType a) => Proxy sh -> Proxy a -> [CInt] -> Segment -> IO String
arrayReferenceStr psh pty shp seg = do
  arr <- Ex.toAccArray psh pty shp seg
  let str = replace "Array" "fromList" $ show arr
  return $ "(use (" ++ str ++ ":: Array " ++ Ex.accShapeAnnot psh ++ " " ++ Ex.accTypeAnnot pty ++ "))"

processReference :: String -> Ptr AccArray -> IO String
processReference ref arr = case refParts of
    ["id",x]    -> case readMaybe x of
      Just i -> getReferenceStr i arr
      _      -> return ref
    ["map"]     -> return "Data.Array.Accelerate.map"
    ["zipwith"] -> return "Data.Array.Accelerate.zipWith"
    ["fold"]    -> return "Data.Array.Accelerate.fold"
    _           -> return ref
  where
  refParts  = Prelude.filter (not . null) $ splitOneOf "() " ref

appendReference :: [String] -> [String]
appendReference [] = []
appendReference (x1:x2:xs) = if (x1 == "(id") then (x1 ++ " " ++ x2) : (appendReference xs) else x1 : (appendReference (x2:xs))
appendReference _ = error "appendReference"

appendList :: [String] -> String
appendList [] = ""
appendList (x:xs) = x ++ " " ++ (appendList xs)

-- | Takes an SExpression in a string, takes payloads, and takes pre-allocated result space.
runAcc :: CString -> Ptr AccArray -> Ptr AccArray -> IO ()
runAcc x arr res = do
  str <- peekCString x
  let exp = splitWs str
  -- putStrLn $ "SExp-containing string received by Haskell: " ++ str
  let exp1 = appendReference exp
  -- putStrLn (show exp1)
  let exp2 = (L.map (\x1 -> let x2 = unsafePerformIO $ (processReference x1 arr) :: [Char]
                            in x2) exp1) :: [[Char]] --(processReference (last exp3) arr)
  -- putStrLn (show exp2)
  let exp3 = appendList exp2
  -- putStrLn (show exp3)
  x1 <- runInterpreter $ setImports ["Prelude", "Data.Array.Accelerate.Interpreter", "Data.Array.Accelerate"] >> eval ("run $ " ++ exp3)
  let values = fromRight x1
  let arr = (last (Ds.split " " values))
  AccArray atyp ashp adata <- peek res
  Segment s1ls s1ts s1da <- peek adata
  a1 <- do
        case (Prelude.fromIntegral s1ts) of
          0 -> do
               let arr' = (read arr) :: [Int]
               -- putStrLn (show arr')
               let a3 = L.map Prelude.fromIntegral arr' :: [CInt]
               pokeArray (castPtr s1da :: Ptr CInt) a3
          1 -> do
               let arr' = (read arr) :: [Double]
               -- putStrLn (show arr')
               let a3 = L.map Prelude.realToFrac arr' :: [CDouble]
               pokeArray (castPtr s1da :: Ptr CDouble) a3
          _ -> putStrLn "Dont Care Case !!!\n"
  new_str <- newCString values
  putStr "-"-- (exp3 ++ "\n")
