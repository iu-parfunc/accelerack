{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Accelerack.Parse where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Language.Sexp
import Text.Read (readEither)
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString(..))

type Parse a = Sexp   -> P a
type Lex   a = String -> L a
type Known   = Map String String
type P       = ReaderT Known L
type L       = Either  String

-- | Collection of supported accelerate ops.
-- 
--   Keys are racket identifier
--   Values are associated accelerate identifer
known :: Known
known = M.fromList
  [ self "map"
  , self "add1"
  ]

self :: String -> (String,String)
self = join (,)

-- | Extend Accelerack syntax by adding cases here.
parseExp :: Parse Exp
parseExp = \case
  -- Parse syntactic forms most to least specific
  --
  -- Syntactic forms --
  List [Atom "id",x] -> ArrIx <$> parseInt x
  -- General forms --
  List es -> App <$> mapM parseExp es
  Atom (str -> x) -> do
    mo <- asks $ M.lookup x
    return $ maybe (Var x) Op mo

data Exp
  = ArrIx Int
  | Var String
  | Op  String
  | App [Exp]
  deriving (Eq,Ord,Show)

runWithKnown :: MonadError String m => Known -> P a -> m a
runWithKnown kn = either throwError return . flip runReaderT kn

lexSexp :: Lex Sexp
lexSexp = either (Left . fst) go . parse . BS.pack
  where
  go = \case
    [s] -> return s
    ss  -> expected "Single SExp" ss

lexSexps :: Lex [Sexp]
lexSexps = either (Left . fst) return . parse . BS.pack

parseInt :: Parse Int
parseInt = \case
  Atom (str -> x) -> lift $ readEither x
  s               -> expected "Int" s

expected_ :: MonadError String m => String -> m a
expected_ ex = expct ex (Nothing :: Maybe ())

expected :: (MonadError String m, Show b) => String -> b -> m a
expected ex = expct ex . Just

expct :: (MonadError String m, Show b) => String -> Maybe b -> m a
expct ex mb = throwError $ "Expected: " ++ ex ++ maybe "" (\b -> ": " ++ show b) mb

str :: ByteString -> String
str = BS.unpack

{-
data Datum
  = Double Double
  deriving (Eq,Ord,Show)

data V
  = VZ Datum
  | VS [V]
  deriving (Eq,Ord,Show)
-}

{-
-- Parse {{{

parseAccel :: String -> Either String AccelExp
parseAccel = parseSexp >=> sexpAccel

sexpAccel :: P AccelExp
sexpAccel = fmap Cmd . sexpCmd

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

readBS :: Read a => String -> ByteString -> Either String a
readBS t = either (const $ expected t) Right . readEither . BS.unpack

-- }}}
-}


{-

C := (use V)

V := (vector T Sh V(T,Sh))

T := _double

Sh := (<int> ...)

V(T,())       := <T>
V(T,(n . ns)) := (v_1 .. v_n), v_i = V(T,ns)

-}

