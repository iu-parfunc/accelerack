{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language PolyKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

module AccArray where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

import Control.Applicative
import Control.Monad

data TypeTag
  = TuplePtrTag
  | GCPtrTag
  | DoubleTag
  | IntTag
  | BoolTag
  | ScalarTag
  | TupleTag
  deriving (Eq,Ord,Show,Bounded)

instance Enum TypeTag where
  toEnum = \case
    1 -> TuplePtrTag
    2 -> GCPtrTag
    3 -> DoubleTag
    4 -> IntTag
    5 -> BoolTag
    6 -> ScalarTag
    7 -> TupleTag
    n -> error $ "unknown TypeTag: " ++ show n
  fromEnum = \case
    TuplePtrTag -> 1 
    GCPtrTag    -> 2 
    DoubleTag   -> 3 
    IntTag      -> 4 
    BoolTag     -> 5 
    ScalarTag   -> 6 
    TupleTag    -> 7 

scalarTag :: TypeTag -> Bool
scalarTag t = elem t
  [ DoubleTag
  , IntTag
  , BoolTag
  ]

type family Tuple (as :: [*]) :: * where
  Tuple '[]  = ()
  Tuple '[a] = a
  Tuple '[a,b] = (a,b)
  Tuple '[a,b,c] = (a,b,c)
  Tuple '[a,b,c,d] = (a,b,c,d)
  Tuple '[a,b,c,d,e] = (a,b,c,d,e)
  Tuple '[a,b,c,d,e,f] = (a,b,c,d,e,f)
  Tuple '[a,b,c,d,e,f,g] = (a,b,c,d,e,f,g)
  Tuple '[a,b,c,d,e,f,g,h] = (a,b,c,d,e,f,g,h)
  Tuple '[a,b,c,d,e,f,g,h,i] = (a,b,c,d,e,f,g,h,i)

data Scalar :: * -> * where
  BoolT   :: Scalar Bool
  IntT    :: Scalar Int
  DoubleT :: Scalar Double

data ArrPtrs :: * where
  Scalar :: Scalar a -> Ptr a -> ArrPtrs
  Tuple  :: [ArrPtrs] -> ArrPtrs

data AccArr = AccArr
  { arrShape :: [Int]
  , arrData  :: ArrPtrs
  }

scalar :: Scalar a -> Ptr b -> ArrPtrs
scalar s = Scalar s . castPtr

peekArrPtrs :: Ptr AccArr -> IO AccArr
peekArrPtrs p = do
  t    <- peekByteOff p 0
  psh  <- peekByteOff p intSize
  ptyp <- peekByteOff p (intSize + ptrSize)
  V tsh szsh psh' <- peek psh
  unless (tsh == IntTag)
    $ fail $ "Bad Shape tag: " ++ show tsh
  sh <- peekArray szsh psh'
  V ttyp sztyp ptyp' <- peek ptyp
  case ttyp of
    TupleTag           -> do
      pts <- peekArray sztyp ptyp'
      _
    _ | scalarTag ttyp -> undefined
    _ -> fail $ "Bad Data tag: " ++ show ttyp

{-
pokeArrPtrs :: Ptr (AccArr a) -> AccArr a -> IO ()
pokeArrPtrs p a = undefined
-}

data V a = V
  { vTag  :: TypeTag
  , vSize :: Int
  , vData :: Ptr a
  }

instance Storable (V a) where
  sizeOf    _ = 2*intSize + ptrSize
  alignment _ = 8
  peek p = V
    <$> (fromCInt <$> peekByteOff p 0)
    <*> (fromCInt <$> peekByteOff p intSize)
    <*> peekByteOff p (2*intSize)
  poke p (V t sz d) = do
    pokeByteOff p 0           $ toCInt t
    pokeByteOff p intSize     $ toCInt sz
    pokeByteOff p (2*intSize) d

intSize :: Int
intSize = sizeOf (undefined :: CInt)

ptrSize :: Int
ptrSize = sizeOf nullPtr

fromCInt :: Enum a => CInt -> a
fromCInt = toEnum . fromEnum

toCInt :: Enum a => a -> CInt
toCInt = toEnum . fromEnum

