{-# LANGUAGE LambdaCase #-}

module Accelerack.Gen where

import Accelerack.Parse
import qualified Data.List as L

genCmd :: Cmd -> String
genCmd c = case c of
  Use v -> annot c $ "use " ++ genVec v

genVec :: Vec -> String
genVec v = genV $ getVec v

genV :: V -> String
genV = \case
  VZ a -> genDatum a
  VS v -> undefined

genDatum :: Datum -> String
genDatum = undefined

annot :: HasType t => t -> String -> String
annot t s = "(" ++ s ++ " :: " ++ type_ t ++ ")"

