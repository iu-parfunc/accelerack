{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

module Accelerack.Marshal where

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.List as L
import GHC.Exts (Constraint)
import Data.Maybe (catMaybes)
import Data.IORef

import Data.Array.Accelerate as A hiding ((++), replicate, product)
import Data.Array.Accelerate.Array.Sugar as A hiding ((++), replicate, product)
import Data.Array.Accelerate.IO (fromPtr, toPtr,BlockPtrs)
import Data.Array.Accelerate.Array.Data
import Data.Typeable

-- Conversions {{{

toAccArray' :: (Shape sh,Elt e)
  => AccShape sh
  -> AccBlockPtrs e
  -> IO (A.Array sh e)
toAccArray' sh ps = fromPtr (getShape sh) (getBlockPtrs ps)

{-
fromAccArray :: forall sh e. (Shape sh, Elt e) => A.Array sh e -> IO (ArrPtrs,IO ())
fromAccArray arr = do
  fin <- newIORef $ return ()
  (aps,bps)  <- allocPtrs fin tr
  toPtr arr bps
  (,) aps <$> readIORef fin
  where
  allocPtrFinal :: Storable a => IORef (IO ()) -> IO (Ptr a)
  allocPtrFinal fin = do
    p <- malloc
    modifyIORef fin (free p >>)
    return p
  allocPtrs :: IORef (IO ()) -> ArrayEltR er -> IO (ArrPtrs,BlockPtrs er)
  allocPtrs fin = \case
    ArrayEltRunit     -> return (ArrPtrs _ $ Tuple [],())
    ArrayEltRint      -> undefined -- allocRecord fin
    ArrayEltRint8     -> undefined -- allocRecord fin
    ArrayEltRint16    -> undefined -- allocRecord fin
    ArrayEltRint32    -> undefined -- allocRecord fin
    ArrayEltRint64    -> undefined -- allocRecord fin
    ArrayEltRword     -> undefined -- allocRecord fin
    ArrayEltRword8    -> undefined -- allocRecord fin
    ArrayEltRword16   -> undefined -- allocRecord fin
    ArrayEltRword32   -> undefined -- allocRecord fin
    ArrayEltRword64   -> undefined -- allocRecord fin
    ArrayEltRcshort   -> undefined -- allocRecord fin
    ArrayEltRcushort  -> undefined -- allocRecord fin
    ArrayEltRcint     -> undefined -- allocRecord fin
    ArrayEltRcuint    -> undefined -- allocRecord fin
    ArrayEltRclong    -> undefined -- allocRecord fin
    ArrayEltRculong   -> undefined -- allocRecord fin
    ArrayEltRcllong   -> undefined -- allocRecord fin
    ArrayEltRcullong  -> undefined -- allocRecord fin
    ArrayEltRfloat    -> undefined -- allocRecord fin
    ArrayEltRdouble   -> undefined -- allocRecord fin
    ArrayEltRcfloat   -> undefined -- allocRecord fin
    ArrayEltRcdouble  -> undefined -- allocRecord fin
    ArrayEltRbool     -> undefined -- allocRecord fin
    ArrayEltRchar     -> undefined -- allocRecord fin
    ArrayEltRcchar    -> undefined -- allocRecord fin
    ArrayEltRcschar   -> undefined -- allocRecord fin
    ArrayEltRcuchar   -> undefined -- allocRecord fin
    ArrayEltRpair a b -> undefined -- (,) <$> allocPtrs fin a <*> allocPtrs fin b
  ----
  mkArrPtrs :: Type (Ptr ()) -> ArrPtrs
  mkArrPtrs = undefined -- ArrPtrs $ dims $ A.arrayShape arr
  dims :: ArrayEltR (EltRepr sh) -> sh -> [Int]
  dims = \case
    ArrayEltRunit                -> \_ -> []
    ArrayEltRpair a ArrayEltRint -> _ -- (,) <$> allocPtrs fin a <*> allocPtrs fin b
    _                            -> error $ "Bad shape repr"
  sh :: ArrayEltR (EltRepr sh)
  sh = arrayElt
  tr :: ArrayEltR (EltRepr e)
  tr = arrayElt
-}

{-
toAccArray :: ArrPtrs -> IO (A.Array DIM1 Int)
toAccArray ArrPtrs {arrShape= [len], arrData = Int ptr } =
  fromPtr (Z :. len) ((), castPtr ptr :: Ptr Int)
-}

{-
fromAccArray :: A.Array DIM1 Int -> IO ArrPtrs
fromAccArray arr =
  do let (Z :. sz) = A.arrayShape arr
     ptr <- mallocArray sz
     toPtr arr ((), ptr)
     return $ ArrPtrs [sz] (Int$ castPtr ptr)
-}

-- }}}

-- AccShape {{{

data AccShape :: * -> * where
  ShZ :: AccShape Z
  ShS :: Int -> AccShape sh -> AccShape (sh :. Int)

someShape :: [Int] -> SomeC Shape AccShape
someShape = \case
  []   -> retC ShZ
  n:ns -> someShape ns >>~ retC . ShS n

getShape :: AccShape sh -> sh
getShape = \case
  ShZ      -> Z
  ShS n sh -> getShape sh :. n

lowerShape :: forall sh e. Array sh e -> [Int]
lowerShape arr = lowerShape' e sh
  where
  e :: ArrayEltR (EltRepr sh)
  e = arrayElt
  sh :: sh
  sh = arrayShape arr

lowerShape' :: ArrayEltR sh -> sh -> [Int]
lowerShape' = \case
  ArrayEltRpair e ArrayEltRint -> \(sh,n) -> n : lowerShape' e sh
  ArrayEltRunit                -> \()     -> []
  _                            -> error "Malformed shape"

-- }}}

-- AccBlockPtrs {{{

data AccBlockPtrs :: * -> * where
  Nil     :: AccBlockPtrs ()
  BoolP   :: Ptr Bool
          -> AccBlockPtrs Bool
  IntP    :: Ptr Int
          -> AccBlockPtrs Int
  DoubleP :: Ptr Double
          -> AccBlockPtrs Double
  Tup2    :: AccBlockPtrs a -> AccBlockPtrs b
          -> AccBlockPtrs (a,b)
  Tup3    :: AccBlockPtrs a -> AccBlockPtrs b
          -> AccBlockPtrs c
          -> AccBlockPtrs (a,b,c)
  Tup4    :: AccBlockPtrs a -> AccBlockPtrs b
          -> AccBlockPtrs c -> AccBlockPtrs d
          -> AccBlockPtrs (a,b,c,d)
  Tup5    :: AccBlockPtrs a -> AccBlockPtrs b
          -> AccBlockPtrs c -> AccBlockPtrs d
          -> AccBlockPtrs e
          -> AccBlockPtrs (a,b,c,d,e)
  Tup6    :: AccBlockPtrs a -> AccBlockPtrs b
          -> AccBlockPtrs c -> AccBlockPtrs d
          -> AccBlockPtrs e -> AccBlockPtrs f
          -> AccBlockPtrs (a,b,c,d,e,f)
  Tup7    :: AccBlockPtrs a -> AccBlockPtrs b
          -> AccBlockPtrs c -> AccBlockPtrs d
          -> AccBlockPtrs e -> AccBlockPtrs f
          -> AccBlockPtrs g
          -> AccBlockPtrs (a,b,c,d,e,f,g)
  Tup8    :: AccBlockPtrs a -> AccBlockPtrs b
          -> AccBlockPtrs c -> AccBlockPtrs d
          -> AccBlockPtrs e -> AccBlockPtrs f
          -> AccBlockPtrs g -> AccBlockPtrs h
          -> AccBlockPtrs (a,b,c,d,e,f,g,h)

someBlockPtrs :: Type (Ptr ()) -> SomeC Elt AccBlockPtrs
someBlockPtrs = \case
  Double p                -> retC $ DoubleP $ castPtr p
  Int    p                -> retC $ IntP    $ castPtr p
  Bool   p                -> retC $ BoolP   $ castPtr p
  Tuple []                -> retC Nil
  Tuple [a]               -> someBlockPtrs a
  Tuple [a,b]             -> someBlockPtrs a >>~ \pa ->
                             someBlockPtrs b >>~ \pb ->
                             retC $ Tup2 pa pb
  Tuple [a,b,c]           -> someBlockPtrs a >>~ \pa ->
                             someBlockPtrs b >>~ \pb ->
                             someBlockPtrs c >>~ \pc ->
                             retC $ Tup3 pa pb pc
  Tuple [a,b,c,d]         -> someBlockPtrs a >>~ \pa ->
                             someBlockPtrs b >>~ \pb ->
                             someBlockPtrs c >>~ \pc ->
                             someBlockPtrs d >>~ \pd ->
                             retC $ Tup4 pa pb pc pd
  Tuple [a,b,c,d,e]       -> someBlockPtrs a >>~ \pa ->
                             someBlockPtrs b >>~ \pb ->
                             someBlockPtrs c >>~ \pc ->
                             someBlockPtrs d >>~ \pd ->
                             someBlockPtrs e >>~ \pe ->
                             retC $ Tup5 pa pb pc pd pe
  Tuple [a,b,c,d,e,f]     -> someBlockPtrs a >>~ \pa ->
                             someBlockPtrs b >>~ \pb ->
                             someBlockPtrs c >>~ \pc ->
                             someBlockPtrs d >>~ \pd ->
                             someBlockPtrs e >>~ \pe ->
                             someBlockPtrs f >>~ \pf ->
                             retC $ Tup6 pa pb pc pd pe pf
  Tuple [a,b,c,d,e,f,g]   -> someBlockPtrs a >>~ \pa ->
                             someBlockPtrs b >>~ \pb ->
                             someBlockPtrs c >>~ \pc ->
                             someBlockPtrs d >>~ \pd ->
                             someBlockPtrs e >>~ \pe ->
                             someBlockPtrs f >>~ \pf ->
                             someBlockPtrs g >>~ \pg ->
                             retC $ Tup7 pa pb pc pd pe pf pg
  Tuple [a,b,c,d,e,f,g,h] -> someBlockPtrs a >>~ \pa ->
                             someBlockPtrs b >>~ \pb ->
                             someBlockPtrs c >>~ \pc ->
                             someBlockPtrs d >>~ \pd ->
                             someBlockPtrs e >>~ \pe ->
                             someBlockPtrs f >>~ \pf ->
                             someBlockPtrs g >>~ \pg ->
                             someBlockPtrs h >>~ \ph ->
                             retC $ Tup8 pa pb pc pd pe pf pg ph
  _                       -> error "Unsupported type"

getBlockPtrs :: AccBlockPtrs e -> BlockPtrs (EltRepr e)
getBlockPtrs = \case
  Nil  {}              -> ()
  BoolP p              -> ((),castPtr p)
  IntP  p              -> ((),castPtr p)
  DoubleP p            -> ((),castPtr p)
  Tup2 a b             -> (getBlockPtrs a                    , getBlockPtrs' b)
  Tup3 a b c           -> (getBlockPtrs (Tup2 a b)           , getBlockPtrs' c)
  Tup4 a b c d         -> (getBlockPtrs (Tup3 a b c)         , getBlockPtrs' d)
  Tup5 a b c d e       -> (getBlockPtrs (Tup4 a b c d)       , getBlockPtrs' e) 
  Tup6 a b c d e f     -> (getBlockPtrs (Tup5 a b c d e)     , getBlockPtrs' f) 
  Tup7 a b c d e f g   -> (getBlockPtrs (Tup6 a b c d e f)   , getBlockPtrs' g) 
  Tup8 a b c d e f g h -> (getBlockPtrs (Tup7 a b c d e f g) , getBlockPtrs' h) 

getBlockPtrs' :: AccBlockPtrs e -> BlockPtrs (EltRepr' e)
getBlockPtrs' = \case
  Nil  {}              -> ()
  BoolP p              -> castPtr p
  IntP  p              -> castPtr p
  DoubleP p            -> castPtr p
  Tup2 a b             -> (getBlockPtrs a,getBlockPtrs' b)
  Tup3 a b c           -> (getBlockPtrs (Tup2 a b)           , getBlockPtrs' c)  
  Tup4 a b c d         -> (getBlockPtrs (Tup3 a b c)         , getBlockPtrs' d)  
  Tup5 a b c d e       -> (getBlockPtrs (Tup4 a b c d)       , getBlockPtrs' e)  
  Tup6 a b c d e f     -> (getBlockPtrs (Tup5 a b c d e)     , getBlockPtrs' f)  
  Tup7 a b c d e f g   -> (getBlockPtrs (Tup6 a b c d e f)   , getBlockPtrs' g)  
  Tup8 a b c d e f g h -> (getBlockPtrs (Tup7 a b c d e f g) , getBlockPtrs' h)  

-- }}}

-- ArrPtrs {{{

data ArrPtrs = ArrPtrs
  { arrShape :: [Int]
  , arrData  :: Type (Ptr ())
  }

data Type ann
  = Double ann
  | Int    ann
  | Bool   ann
  | Tuple [Type ann]
  deriving (Eq,Ord,Show)

peekArrPtrs :: Ptr ArrPtrs -> IO ArrPtrs
peekArrPtrs p = do
  psh  <- peekByteOff p  intSize
  pdat <- peekByteOff p (intSize + ptrSize)
  ArrPtrs <$> peekShape psh <*> peekTypeData pdat

peekShape :: Ptr Segment -> IO [Int]
peekShape = peek >=> \(Segment szsh tsh psh) -> do
  unless (tsh == IntTag) $
    fail $ "Bad Shape tag: " ++ show tsh
  peekArray szsh $ castPtr psh

peekTypeData :: Ptr Segment -> IO (Type (Ptr ()))
peekTypeData = peek >=> \(Segment sztyp ttyp ptyp) -> do
  case ttyp of
    IntTag    -> return $ Int    ptyp
    DoubleTag -> return $ Double ptyp
    BoolTag   -> return $ Bool   ptyp
    TupleTag  -> do
      pts <- peekArray sztyp $ castPtr ptyp
      Tuple <$> mapM peekTypeData pts

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

-- }}}

-- Segment {{{

data Segment = Segment
  { vSize :: Int
  , vTag  :: TypeTag
  , vData :: Ptr ()
  }

data TypeTag
  = IntTag      -- 0
  | DoubleTag   -- 1
  | BoolTag     -- 2
  | TupleTag    -- 3
  | Other Int
  deriving (Eq,Ord,Show)

instance Enum TypeTag where
  fromEnum = \case
    IntTag    -> 0
    DoubleTag -> 1
    BoolTag   -> 2
    TupleTag  -> 3
    Other n   -> n
  toEnum = \case
    0 -> IntTag
    1 -> DoubleTag
    2 -> BoolTag
    3 -> TupleTag
    n -> Other n

newtype Payload = Payload
  { getPayload :: Ptr ()
  }

instance Storable Segment where
  sizeOf    _ = 2*intSize + ptrSize
  alignment _ = 8
  peek p = Segment
    <$> (fromCInt <$> peekByteOff p 0)
    <*> (fromCInt <$> peekByteOff p intSize)
    <*> peekByteOff p (2*intSize)
  poke p (Segment sz t d) = do
    pokeByteOff p 0           $ toCInt sz
    pokeByteOff p intSize     $ toCInt t
    pokeByteOff p (2*intSize)   d

-- }}}

-- Util {{{

data SomeC (c :: k -> Constraint) (f :: k -> *) :: * where
  SomeC :: c a => f a -> SomeC c f

(>>~) :: SomeC c f -> (forall a. c a => f a -> r) -> r
SomeC a >>~ f = f a
infixl 1 >>~

retC :: c a => f a -> SomeC c f
retC = SomeC

toCInt :: Enum a => a -> CInt
toCInt = toEnum . fromEnum

fromCInt :: Enum a => CInt -> a
fromCInt = toEnum . fromEnum

intSize, ptrSize :: Int
intSize = sizeOf (undefined :: CInt)
ptrSize = sizeOf nullPtr

-- }}}

