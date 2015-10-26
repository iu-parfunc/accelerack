{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Accelerack.Parse where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Language.Sexp
import Text.Read (readEither)
import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Parse  a = Sexp   -> P a
type Lex    a = String -> L a
type KnownOps = Map String String
type P        = ReaderT KnownOps L
type L        = Either  String

-- | Collection of supported accelerate ops.
-- 
--   Keys are racket identifier
--   Values are associated accelerate identifer
known :: KnownOps
known = M.fromList
  [ self "map"
  , self "add1"
  ]

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

parseInt :: Parse Int
parseInt = \case
  Atom (str -> x) -> lift $ readEither x
  s               -> expected "Int" s

data Exp
  = ArrIx Int
  | Var String
  | Op  String
  | App [Exp]
  deriving (Eq,Ord,Show)

runWithKnown :: MonadError String m => KnownOps -> P a -> m a
runWithKnown kn = either throwError return . flip runReaderT kn

lexSexp :: Lex Sexp
lexSexp = either (Left . fst) go . parse . BS.pack
  where
  go = \case
    [s] -> return s
    ss  -> expected "Single SExp" ss

lexSexps :: Lex [Sexp]
lexSexps = either (Left . fst) return . parse . BS.pack

-- Util {{{

expected_ :: MonadError String m => String -> m a
expected_ ex = expct ex (Nothing :: Maybe ())

expected :: (MonadError String m, Show b) => String -> b -> m a
expected ex = expct ex . Just

expct :: (MonadError String m, Show b) => String -> Maybe b -> m a
expct ex mb = throwError $ "Expected: " ++ ex ++ maybe "" (\b -> ": " ++ show b) mb

str :: ByteString -> String
str = BS.unpack

self :: String -> (String,String)
self = join (,)

-- }}}

