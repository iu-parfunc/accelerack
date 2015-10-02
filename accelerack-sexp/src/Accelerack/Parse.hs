{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad

test :: String
test = "(use (vector _double (2 3) ((1 2 3) (4 5 6))))"

type P a = Sexp -> Either String a

data Block
  = Cmd Cmd
  deriving (Eq,Ord,Show)

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

data Datum
  = Double Double
  deriving (Eq,Ord,Show)

data V
  = VZ Datum
  | VS [V]
  deriving (Eq,Ord,Show)

-- Parse {{{

parseAccel :: String -> Either String Block
parseAccel = parseSexp >=> sexpBlock

parseSexp :: String -> Either String Sexp
parseSexp = either (Left . fst) go . parse . BS.pack
  where
  go = \case
    [s] -> return s
    _   -> expected "Single SExp"

sexpBlock :: P Block
sexpBlock = fmap Cmd . sexpCmd

sexpCmd :: P Cmd
sexpCmd = \case
  List [Atom "use",v] -> Use <$> sexpVec v
  _                   -> expected "Cmd"

sexpVec :: P Vec
sexpVec = \case
  List [Atom "vector",t,sh,v] -> do
    t'  <- sexpType t
    sh' <- sexpShape sh
    v'  <- sexpV t' sh' v
    return $ Vec t' sh' v'
  _ -> expected "Vec"

sexpType :: P Type
sexpType = \case
  Atom s | s == "_double" -> return Dbl
  _      -> expected "Type"

sexpInt :: P Int
sexpInt = \case
  Atom s -> readBS "Int" s
  _      -> expected "Int"

sexpShape :: P Shape
sexpShape = \case
  List ss -> Shape <$> mapM sexpInt ss
  _       -> expected "Shape"

sexpDbl :: P Double
sexpDbl = \case
  Atom s -> readBS "Double" s
  _      -> expected "Double"

sexpDatum :: Type -> P Datum
sexpDatum = \case
  Dbl -> fmap Double . sexpDbl

sexpV :: Type -> Shape -> P V
sexpV t = \case
  Shape [] -> fmap VZ . sexpDatum t
  Shape (n:ns) -> \case
    List ss | length ss == n
      -> do vs <- mapM (sexpV t $ Shape ns) ss
            return $ VS vs
    _ -> expected $ "List of length " ++ show n

expected :: String -> Either String a
expected = Left . ("Expected: " ++)

readBS :: Read a => String -> ByteString -> Either String a
readBS t = either (const $ expected t) Right . readEither . BS.unpack

-- }}}


{-

C := (use V)

V := (vector T Sh V(T,Sh))

T := _double

Sh := (<int> ...)

V(T,())       := <T>
V(T,(n . ns)) := (v_1 .. v_n), v_i = V(T,ns)

-}

