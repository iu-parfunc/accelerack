{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- For Typeable of Extend:
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE TypeFamilies #-}

import Control.Monad (forM_)
import Data.Dynamic

--------------------------------------------------------------------------------
-- (*) Types

-- | Simplest version.  Use a closed world of value types in the Exp:
data Ty = BoolTy | IntTy | AnyTy
  deriving (Show, Typeable)
-- The way Accelerate, for example, does this is more complicated,
-- using a class for value types which can reify them.

deriving instance (Typeable 'BoolTy)
deriving instance (Typeable 'IntTy)
deriving instance (Typeable 'AnyTy)

class Typeable t => ReifyTy t where
  reifyTy :: Proxy t -> Ty
instance ReifyTy 'BoolTy where reifyTy _ = BoolTy
instance ReifyTy 'IntTy  where reifyTy _ = IntTy
instance ReifyTy 'AnyTy  where reifyTy _ = AnyTy

--------------------------------------------------------------------------------
-- (*) Environments

data Env = EmptyEnv
         | forall a . Extend a Env

deriving instance (Typeable 'EmptyEnv)
deriving instance Typeable 'Extend

type family   ENV_HEAD (t::Env) :: Ty
type instance ENV_HEAD ('Extend s e) = s

--------------------------------------------------------------------------------
-- (1) GADT based AST 

data Exp (env :: Env) (a :: Ty) where
  T   :: Exp env BoolTy
  F   :: Exp env BoolTy
  If  :: Exp env BoolTy -> Exp env a -> Exp env a -> Exp env a
  Lit :: Int -> Exp env IntTy
  Add :: Exp env IntTy -> Exp env IntTy -> Exp env IntTy
  Let :: ReifyTy t1 =>
         Exp env t1
      -> Exp (Extend t1 env) a
      -> Exp env a  
  Var :: Idx env a -> Exp env a
 deriving Typeable

data Idx (env :: Env) (t :: Ty) where
  -- Here, again, ReifyTy is redundant, but no way to tell GHC that:
  Zero :: forall (env :: Env) (t::Ty) . ReifyTy t =>
          Idx (Extend t env) t
  Succ :: forall (env :: Env) (s :: Ty) (t :: Ty) . ReifyTy s => 
          Idx env t -> Idx (Extend s env) t
  deriving Typeable

deriving instance (Show (Exp env a))

deriving instance (Show (Idx env t))

--------------------------------------------------------------------------------
-- (2) ADT version:

-- | Strip all phantom type args. 
--   (BUT... it could reify them all instead?)
data Exp2 = T2 
          | F2
          | If2 Exp2 Exp2 Exp2
          | Lit2 Int
          | Add2 Exp2 Exp2 
          | Let2 Exp2 Exp2
          | Var2 Idx2
  deriving (Show, Typeable)

-- | Reify the type arg to the value level:
data Idx2 = Zero2 Ty
          | Succ2 Ty Idx2
  deriving (Show, Typeable)

--------------------------------------------------------------------------------

-- | Downcasting never fails.  It strips type-level information or
-- reifies it to the value level.
downcast :: ReifyTy a => Exp env a -> Exp2
downcast e =
  case e of
    Lit n -> Lit2 n
    T  -> T2
    F  -> F2
    If a b c -> If2 (downcast a) (downcast b) (downcast c)
    Add a b -> Add2 (downcast a) (downcast b)
    Let a b -> Let2 (downcast a) (downcast b) 
    Var ix  -> Var2 (downcastIdx ix)

downcastIdx :: forall env1 a1 . ReifyTy a1 =>
               Idx env1 a1 -> Idx2
downcastIdx Zero = Zero2 (reifyTy (Proxy :: Proxy (ENV_HEAD env1)))
downcastIdx (Succ (inner :: Idx env2 a1)) =
  Succ2 (reifyTy (Proxy :: Proxy (ENV_HEAD env1)))
        (downcastIdx inner)

--------------------------------------------------------------------------------
-- Option 1: the old way.  Sealed, monomorphic data and Data.Dynamic.

-- Typeable constraints are actually redundant here, because the kind
-- of 'env' and 'a' should imply Typeable, but we have no way to
-- express that.
data Sealed = forall env a . (Typeable env, ReifyTy a) =>
              Sealed (Exp env a)

data SealedIdx = forall env a . (Typeable env, ReifyTy a) =>
                 SealedIdx (Idx env a)

data SealedTy = forall (t :: Ty) . ReifyTy t =>
                SealedTy (Proxy t)

-- | The inverse of reifyTy: value to type level.
toType :: Ty -> SealedTy
toType IntTy  = SealedTy (Proxy :: Proxy 'IntTy)
toType BoolTy = SealedTy (Proxy :: Proxy 'BoolTy)
toType AnyTy  = SealedTy (Proxy :: Proxy 'AnyTy)
                                  

-- upcastIdx :: Typeable a => Idx2 -> Idx env a
upcastIdx :: Idx2 -> SealedIdx
upcastIdx (Zero2 ty) =
  case toType ty of
    SealedTy (_ :: Proxy tty) -> 
      SealedIdx (Zero :: Idx ('Extend tty 'EmptyEnv) tty)
upcastIdx (Succ2 ty ix2) =
  case (toType ty, upcastIdx ix2) of
    (SealedTy (_ :: Proxy tty),
     SealedIdx (ixb :: Idx env2 a2)) -> 
      SealedIdx (Succ ixb :: Idx ('Extend tty env2) a2)


-- | Only closed expressions here:
upcast1 :: forall a . Typeable a => Exp2 -> Exp EmptyEnv a
-- upcast1 :: forall a . Exp2 -> Ty -> Maybe (Exp EmptyEnv a)
upcast1 exp2 =
  case go exp2 of Sealed e -> safeCast e
 where
  go :: Exp2 -> Sealed
  go e2 =
    case e2 of
     T2 -> Sealed (T :: Exp EmptyEnv BoolTy)
     F2 -> Sealed (F :: Exp EmptyEnv BoolTy)
     If2 x1 x2 x3 ->
       case (go x1, go x2, go x3) of
         (Sealed (a::Exp env1 t1),
          Sealed (b::Exp env2 t2),
          Sealed c) -> Sealed $
           -- FIXME: Need to somehow COMBINE the environments:
           If (safeCast a :: Exp env1 BoolTy)
              (safeCast b :: Exp env1 t2)
              (safeCast c :: Exp env1 t2)
     Lit2 x -> Sealed (Lit x :: Exp EmptyEnv IntTy)
     Add2 x1 x2 ->
       case (go x1, go x2) of
         (Sealed (a::Exp env1 t1), Sealed b) -> 
          Sealed $ Add (safeCast a :: Exp env1 IntTy)
                       (safeCast b :: Exp env1 IntTy)
     Var2 x -> case upcastIdx x of
                 SealedIdx ix -> Sealed (Var ix)
     
     Let2 x1 x2 ->
       case (go x1, go x2) of
         (Sealed (a::Exp env1 t1),
          Sealed (b::Exp env2 t2)) -> 
           Sealed 
            (Let a (safeCast b :: Exp (Extend t1 env1) t2)
             :: Exp env1 t2)


safeCast :: forall a b . (Typeable a, Typeable b) => a -> b
safeCast a =
  case fromDynamic (toDyn a) of
    Just x -> x
    Nothing -> error $ "safeCast failed, from "++show (typeOf (unused::a))++
                       " to "++show (typeOf (unused::b))
    

--------------------------------------------------------------------------------
-- Option 2: the new way.  Consumer demands the type and we upcast
-- without ever sealing.

-- FINISHME   

--------------------------------------------------------------------------------
-- Misc + Test programs:

unused :: a
unused = error "This value should never be used"

instance Show Sealed where
  show (Sealed x) = "<Sealed: "++show (typeOf x)++">"

instance Show SealedIdx where
  show (SealedIdx x) = "<SealedIdx: "++show (typeOf x)++">"

instance Show SealedTy where
  show (SealedTy x) = "<SealedTy: "++show (typeOf x)++">"

p0 :: Exp EmptyEnv IntTy
p0 = If T (Lit 3) (Lit 4)

t_p0 :: Exp EmptyEnv IntTy
t_p0 = upcast1 (downcast p0) 

p1 :: Exp EmptyEnv IntTy
p1 = Let (Lit 5) (Var Zero)

p2 :: Exp EmptyEnv IntTy
p2 = (If T (Lit 11) p1)

p3 :: Exp EmptyEnv IntTy
p3 = Let (Lit 5) 
      (If T (Var Zero) (Lit 4))

p3b :: Exp2
p3b = Let2 (Lit2 5) 
      (If2 T2 (Var2 (Zero2 IntTy)) (Lit2 4))



i0 :: Idx (Extend IntTy (Extend BoolTy EmptyEnv)) BoolTy
i0 = Succ Zero 

t_i0 :: IO ()
t_i0 = print $ upcastIdx $ downcastIdx i0

--------------------------------------------------------------------------------

-- FinishMe: test more uniformly:
tests :: [(String,Sealed)]
tests = [("p0",Sealed p0),
         ("p1",Sealed p1),
         ("p2",Sealed p2),
         ("p3",Sealed p3)]

main :: IO ()
main = do
  putStrLn "\nTest i0:"
  t_i0

  forM_ tests $ \ (name, Sealed (expr::Exp env a)) -> do
    putStrLn$ "\nTest "++name++":"
    putStrLn$ "  Orig: "++show expr
    putStrLn$ "  Down: "++show (downcast expr)
    putStrLn$ "  BkUp: "++show (upcast1 (downcast expr) :: Exp EmptyEnv a)
