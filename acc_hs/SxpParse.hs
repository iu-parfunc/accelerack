{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SxpParse where

import Foreign.C (CString)
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as StrictBS
import Language.Sexp.Parser (Sexp(..),parse)
import Control.Monad
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data AccSxp
  = AccRef  Int
  | AccAtom String
  | AccApp  [AccSxp]
  deriving (Eq,Ord,Show)

parseAccSxp :: [(String,String)] -> CString -> IO AccSxp
parseAccSxp known cs = do
  bs <- BS.fromStrict <$> StrictBS.packCString cs
  case parse bs of
    Left (err,_) -> parseFail err
    Right [sxp]  -> go sxp
    Right ss     -> parseFail $ "expected single Sexp, but got: " ++ show ss
  where
  go :: Sexp -> IO AccSxp
  go = \case
    List ["id",Atom (BS.unpack -> x)] ->
      case readMaybe x of
        Just i ->
          return $ AccRef i
        _      ->
          parseFail $ "couldn't read Int: " ++ show x
    List es ->
      AccApp <$> traverse go es
    Atom (BS.unpack -> a) ->
      return $ AccAtom $ fromMaybe a $ lookup a known
    
parseFail :: String -> IO a
parseFail msg = fail $ "Parse failure: " ++ msg

