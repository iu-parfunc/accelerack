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

import Data.Dynamic

--------------------------------------------------------------------------------
-- (1) GADT based AST 

data Env = EmptyEnv | forall a . Extend a Env
-- deriving Typeable

deriving instance (Typeable 'EmptyEnv)
-- deriving instance (Typeable a, Typeable e) => (Typeable ('Extend a e))
deriving instance Typeable 'Extend

-- | Simplest version.  Use a closed world of value types in the Exp:
data Ty = BoolTy | IntTy | AnyTy
  deriving (Show, Typeable)
-- The way Accelerate, for example, does this is more complicated,
-- using a class for value types which can reify them.

deriving instance (Typeable 'BoolTy)
deriving instance (Typeable 'IntTy)
deriving instance (Typeable 'AnyTy)

class ReifyTy t where
  reifyTy :: Idx env t -> Ty
instance ReifyTy 'BoolTy where reifyTy _ = BoolTy
instance ReifyTy 'IntTy  where reifyTy _ = IntTy
instance ReifyTy 'AnyTy  where reifyTy _ = AnyTy


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

-- deriving instance Typeable (Exp env a)

data Idx (env :: Env) (t :: Ty) where
  Zero ::              Idx (Extend t env) t
  Succ :: Idx env t -> Idx (Extend s env) t

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

--------------------------------------------------------------------------------
-- Type and environment handling

downcastIdx :: forall env1 a1 . ReifyTy a1 =>
               Idx env1 a1 -> Idx2
downcastIdx Zero = Zero2 undefined
downcastIdx (Succ (inner :: Idx env2 a2) :: Idx env1 a1) =
  Succ2 (reifyTy inner)
  (downcastIdx inner)

--------------------------------------------------------------------------------
-- Option 1: the old way.  Sealed, monomorphic data and Data.Dynamic.
    
data Sealed = forall env a . (Typeable env, Typeable a) =>
              Sealed (Exp env a)
              -- { unseal :: Exp env a } -- Dynamic

data SealedIdx = forall env a . (Typeable env, Typeable a) =>
                 SealedIdx (Idx env a)

data SealedTy = forall (t :: Ty) . Typeable t =>
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
  undefined


unused :: a
unused = error "This value should never be used"

test :: Sealed -> Dynamic
test (Sealed x) = toDyn x

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
           If (safeCast a :: Exp env1 BoolTy)
              (safeCast b :: Exp env1 t2)
              (safeCast c :: Exp env1 t2)
     Lit2 x -> Sealed (Lit x :: Exp EmptyEnv IntTy)
     Add2 x1 x2 ->
       case (go x1, go x2) of
         (Sealed (a::Exp env1 t1), Sealed b) -> 
          Sealed $ Add (safeCast a :: Exp env1 IntTy)
                       (safeCast b :: Exp env1 IntTy)
     Var2 x -> undefined 
       -- case upcastIdx x of
       --   _ -> undefined
     
     Let2 x1 x2 -> undefined


safeCast :: forall a b . (Typeable a, Typeable b) => a -> b
safeCast a =
  case fromDynamic (toDyn a) of
    Just x -> x
    Nothing -> error $ "safeCast failed, from "++show (typeOf (undefined::a))++
                       " to "++show (typeOf (undefined::b))
    

--------------------------------------------------------------------------------
-- Option 2: the new way.  Consumer demands the type and we downcast
-- without ever sealing.

-- FINISHME   

--------------------------------------------------------------------------------
-- Test programs:

p0 :: Exp EmptyEnv IntTy
p0 = If T (Lit 3) (Lit 4)

t0 :: Exp EmptyEnv IntTy
t0 = upcast1 (downcast p0) 

p1a :: Exp EmptyEnv IntTy
p1a = Let (Lit 5) 
      (If T (Var Zero) (Lit 4))

p1b :: Exp2
p1b = Let2 (Lit2 5) 
      (If2 T2 (Var2 (Zero2 IntTy)) (Lit2 4))

--------------------------------------------------------------------------------

tests :: [Sealed]
tests = [Sealed p0, Sealed p1a]

main :: IO ()
main = do
          putStrLn "\np0:"
          print p0
          print (downcast p0)
          print (upcast1 (downcast p0) :: Exp EmptyEnv IntTy)
          
          putStrLn "\np1a:"
          print p1a
          print (downcast p1a)
          print (upcast1 (downcast p1a) :: Exp EmptyEnv IntTy)

          putStrLn "\np1b:"
          print p1b
