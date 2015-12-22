{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
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

getRef :: Ptr AccArray -> Int -> IO String
getRef arrs refNum = do
  let ptrSize = 8 :: Int
  AccArray atyp ashp adata <- peek arrs
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
            0 -> arrayRef psh (Proxy :: Proxy Int32)
            1 -> arrayRef psh (Proxy :: Proxy Double)
        1 -> let psh = (Proxy :: Proxy DIM1)
          in case s1ts' of
            0 -> arrayRef psh (Proxy :: Proxy Int32)
            1 -> arrayRef psh (Proxy :: Proxy Double)
        2 -> let psh = (Proxy :: Proxy DIM2)
          in case s1ts' of
            0 -> arrayRef psh (Proxy :: Proxy Int32)
            1 -> arrayRef psh (Proxy :: Proxy Double)
        3 -> let psh = (Proxy :: Proxy DIM3)
          in case s1ts' of
            0 -> arrayRef psh (Proxy :: Proxy Int32)
            1 -> arrayRef psh (Proxy :: Proxy Double)
        4 -> let psh = (Proxy :: Proxy DIM4)
          in case s1ts' of
            0 -> arrayRef psh (Proxy :: Proxy Int32)
            1 -> arrayRef psh (Proxy :: Proxy Double)
        5 -> let psh = (Proxy :: Proxy DIM5)
          in case s1ts' of
            0 -> arrayRef psh (Proxy :: Proxy Int32)
            1 -> arrayRef psh (Proxy :: Proxy Double)
        6 -> let psh = (Proxy :: Proxy DIM6)
          in case s1ts' of
            0 -> arrayRef psh (Proxy :: Proxy Int32)
            1 -> arrayRef psh (Proxy :: Proxy Double)
  arrayStrFn sh1 segdata

arrayRef :: (Ex.AccelerackShape sh, Ex.AccelerackType a) => Proxy sh -> Proxy a -> [CInt] -> Segment -> IO String
arrayRef psh pty shp seg = do
  arr <- Ex.toAccArray psh pty shp seg
  let str = replace "Array" "fromList" $ show arr
  return $ "(use (" ++ str ++ ":: Array " ++ Ex.accShapeAnnot psh ++ " " ++ Ex.accTypeAnnot pty ++ "))"

-- | Takes an SExpression in a string, takes payloads, and takes pre-allocated result space.
runAcc :: CString -> Ptr AccArray -> Ptr AccArray -> IO ()
runAcc cs arrs res = do
  sxp <- Parse.parseAccSxp knownAccOps cs
  acc <- insertRefs arrs sxp
  eInterp <- runInterpreter $ do
    setImports ["Prelude", "Data.Array.Accelerate.Interpreter", "Data.Array.Accelerate"]
    eval $ "run $ " ++ acc
  case eInterp of
    Left err -> fail $ show err
    Right values -> do
      AccArray atyp ashp adata <- peek res
      Segment s1ls s1ts s1da <- peek adata
      let pokeSegDataFn = case Prelude.fromIntegral s1ts of
            0 -> let pty = (Proxy :: Proxy CInt)    in pokeSegmentData pty
            1 -> let pty = (Proxy :: Proxy CDouble) in pokeSegmentData pty
      pokeSegDataFn s1da $ last $ words values

pokeSegmentData :: forall ty. (Read ty, Storable ty) => Proxy ty -> Ptr () -> String -> IO ()
pokeSegmentData _ p str = pokeArray (castPtr p :: Ptr ty) (read str :: [ty])

insertRefs :: Ptr AccArray -> Parse.AccSxp -> IO String
insertRefs arrs = \case
  Parse.AccRef i  -> getRef arrs i
  Parse.AccApp es -> unwords . map wrapParens <$> traverse (insertRefs arrs) es
  Parse.AccAtom a -> return a

wrapParens :: String -> String
wrapParens a = concat ["(",a,")"]

knownAccOps :: [(String,String)]
knownAccOps =
  [ ( "map"     , "Data.Array.Accelerate.map" )
  , ( "fold"    , "Data.Array.Accelerate.fold" )
  , ( "zipwith" , "Data.Array.Accelerate.zipWith" )
  ]

