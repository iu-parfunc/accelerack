{-# LANGUAGE LambdaCase #-}

module Accelerack.Marshal where

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import Control.Applicative
import Control.Monad
import qualified Data.List as L

peekArrPtrs :: Ptr a -> IO ArrPtrs
peekArrPtrs p = do
  psh  <- peekByteOff p  intSize
  ptyp <- peekByteOff p (intSize + ptrSize)
  ArrPtrs <$> peekShape psh <*> peekType ptyp

peekShape :: Ptr a -> IO [Int]
peekShape p = do
  Segment tsh szsh psh <- peek $ castPtr p
  unless (tsh == IntTag) $
    fail $ "Bad Shape tag: " ++ show tsh
  peekArray szsh $ castPtr psh

peekType :: Ptr a -> IO (Type (Ptr ()))
peekType p = do
  Segment ttyp sztyp ptyp <- peek $ castPtr p
  case ttyp of
    IntTag    -> return $ Int    ptyp
    DoubleTag -> return $ Double ptyp
    BoolTag   -> return $ Bool   ptyp
    TupleTag  -> do
      pts <- peekArray sztyp $ castPtr ptyp
      Tuple <$> mapM peekType pts

data ArrPtrs = ArrPtrs
  { arrShape :: [Int]
  , arrData  :: Type (Ptr ())
  }

printArrPtrs :: ArrPtrs -> IO ()
printArrPtrs a = do
  mapM_ putStrLn
    [ "Array Shape:"
    , renderShape sh
    , replicate 20 '*' ++ "\n"
    , "Array Type:"
    , renderType dat
    , replicate 20 '*' ++ "\n"
    , "Array Payloads:"
    ]
  ds <- renderData (product sh) dat
  putStrLn ds
  putStrLn $ replicate 20 '*' ++ "\n"
  where
  sh  = arrShape a
  dat = arrData  a

renderShape :: [Int] -> String
renderShape = \case
  []   -> "Z"
  d:ds -> "(" ++ renderShape ds ++ " :. " ++ show d ++ ")"

renderType :: Type a -> String
renderType = \case
  Double _ -> "Double"
  Int    _ -> "Int"
  Bool   _ -> "Bool"
  Tuple ts -> "(" ++ L.intercalate "," (renderType <$> ts) ++ ")"

renderData :: Int -> Type (Ptr ()) -> IO String
renderData len = \case
  Double p -> do
    ds <- peekArray len (castPtr p :: Ptr Double)
    return $ show ds
  Int    p -> do
    is <- peekArray len (castPtr p :: Ptr Int)
    return $ show is
  Bool   p -> do
    bs <- peekArray len (castPtr p :: Ptr Bool)
    return $ show bs
  Tuple ts -> do
    ss <- mapM (renderData len) ts
    return $ "{" ++ L.intercalate ";" ss ++ "}"

data Type ann
  = Double ann
  | Int    ann
  | Bool   ann
  | Tuple [Type ann]
  deriving (Eq,Ord,Show)

data TypeTag
  = IntTag      -- 0
  | DoubleTag   -- 1
  | BoolTag     -- 2
  | TupleTag    -- 3
  deriving (Eq,Ord,Show,Bounded,Enum)

data Segment = Segment
  { vTag  :: TypeTag
  , vSize :: Int
  , vData :: Ptr ()
  }

instance Storable Segment where
  sizeOf    _ = 2*intSize + ptrSize
  alignment _ = 8
  peek p = Segment
    <$> (fromCInt <$> peekByteOff p 0)
    <*> (fromCInt <$> peekByteOff p intSize)
    <*> peekByteOff p (2*intSize)
  poke p (Segment t sz d) = do
    pokeByteOff p 0           $ toCInt t
    pokeByteOff p    intSize  $ toCInt sz
    pokeByteOff p (2*intSize)   d

toCInt :: Enum a => a -> CInt
toCInt = toEnum . fromEnum

fromCInt :: Enum a => CInt -> a
fromCInt = toEnum . fromEnum

intSize, ptrSize :: Int
intSize = sizeOf (undefined :: CInt)
ptrSize = sizeOf nullPtr



