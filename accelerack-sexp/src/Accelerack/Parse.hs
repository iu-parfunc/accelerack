{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Accelerack.Parse where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Language.Sexp
import Text.Read (readEither)

{-
readBS :: Read a => String -> ByteString -> Either String a
readBS expected = either (const $ Left expected) Right . readEither . BS.unpack
-}

str :: ByteString -> String
str = BS.unpack

data Cmd
  = Use Vec
  deriving (Eq,Ord,Show)

data Vec = Vec
  { vecType  :: Type
  , vecShape :: Shape
  , getVec   :: V
  } deriving (Eq,Ord,Show)

data Type
  = Dbl
  deriving (Eq,Ord,Show)

newtype Shape = Shape
  { getShape :: [Int]
  } deriving (Eq,Ord,Show)

type Datum = String

data V
  = VZ Datum
  | VS [V]
  deriving (Eq,Ord,Show)

{-

C := (use V)

V := (vector T Sh V(T,Sh))

T := _double

Sh := (<int> ...)

V(T,())       := <T>
V(T,(n . ns)) := (v_1 .. v_n), v_i = V(T,ns)

-}

