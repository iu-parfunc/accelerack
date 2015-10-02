{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Accelerack.Gen where

import Data.Array.Accelerate hiding ((++))
import Data.Array.Accelerate.Interpreter
import Accelerack.Parse hiding (Type,Shape)
import qualified Accelerack.Parse as P
import qualified Data.List as L
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr

runTest :: IO String
runTest = case parseAccel test of
  Left err -> fail $ "Error: " ++ err
  Right b  -> qprint $ genBlock b

qprint :: Ppr a => Q a -> IO String
qprint q = do
  a <- runQ q
  return $ pprint a

genBlock :: Block -> ExpQ
genBlock b = case b of
  Cmd c -> [| run $(genCmd c) |]

genCmd :: Cmd -> ExpQ
genCmd c = case c of
  Use v -> annot c [| use $(genVec v) |]

genVec :: Vec -> ExpQ
genVec vec@(Vec t sh v) = annot vec [| fromList $(genShape sh) $es |]
  where
  ve = annot t es
  es = listE $ genV sh v

genV :: P.Shape -> V -> [ExpQ]
genV sh = \case
  VZ a -> [genDatum a]
  VS v -> concatMap (genV sh) v

genShape :: P.Shape -> ExpQ
genShape = go . getShape
  where
  go = \case
    []   -> [| Z |]
    n:ns -> [| (n :: Int) :. $(go ns) |]

genDatum :: Datum -> ExpQ
genDatum da = case da of
  Double d -> annot da [| d |]

annot :: HasType t => t -> ExpQ -> ExpQ
annot t s = sigE s $ type_ t

class HasType t where
  type_ :: t -> TypeQ

instance HasType Cmd where
  type_ = \case
    Use v -> [t| Acc $(type_ v) |]

instance HasType Vec where
  type_ (Vec t sh _) = [t| Array $(type_ sh) $(type_ t) |]

instance HasType P.Type where
  type_ = \case
    Dbl -> [t| Double |]

instance HasType P.Shape where
  type_ = go . getShape
    where
    go :: [Int] -> TypeQ
    go = \case
      []   -> [t| Z |]
      _:ns -> [t| Int :. $(go ns) |]

instance HasType Datum where
  type_ = \case
    Double _ -> [t| Double |]

