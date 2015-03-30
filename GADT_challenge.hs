{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- For Typeable of Extend:
{-# LANGUAGE PolyKinds #-}

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

data Exp (env :: Env) (a :: Ty) where
  T   :: Exp env BoolTy
  F   :: Exp env BoolTy
  If  :: Exp env BoolTy -> Exp env a -> Exp env a -> Exp env a
  Lit :: Int -> Exp env IntTy
  Add :: Exp env IntTy -> Exp env IntTy -> Exp env IntTy
  Let :: Exp env t1
      -> Exp (Extend t1 env) a
      -> Exp env a  
  Var :: Idx env a -> Exp env a
 deriving Typeable

-- deriving instance Typeable (Exp env a)

data Idx (env :: Env) t where
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

-- | Same algorithm applied to Idx:
data Idx2 = Zero2 | Succ2 Ty Idx2
  deriving (Show, Typeable)

--------------------------------------------------------------------------------

-- | Downcasting never fails.  It strips type-level information or
-- reifies it to the value level.
downcast :: Exp env a -> Exp2
downcast e =
  case e of
    Lit n -> Lit2 n
    T  -> T2
    F  -> F2
    If a b c -> If2 (downcast a) (downcast b) (downcast c)
    Add a b -> Add2 (downcast a) (downcast b)
    Let a b -> Let2 (downcast a) (downcast b) 
    Var ix ->
      let loop :: forall env t . Idx env t -> Idx2
          loop Zero = Zero2
          loop (Succ idx) = Succ2 (reifyTy (undefined)) (loop idx)
      in Var2 $ loop ix 

reifyTy :: t
reifyTy = undefined

--------------------------------------------------------------------------------
-- Option 1: the old way.  Sealed, monomorphic data and Data.Dynamic.
    
data Sealed = forall env a . (Typeable env, Typeable a) =>
              Sealed (Exp env a)
              -- { unseal :: Exp env a } -- Dynamic

unused :: a
unused = error "This value should never be used"

test :: Sealed -> Dynamic
test (Sealed x) = toDyn x

upcastIdx :: Idx2 -> Idx env a
upcastIdx = undefined

-- | Only closed expressions here:
upcast1 :: forall a . Typeable a => Exp2 -> Exp EmptyEnv a
-- upcast1 :: forall a . Exp2 -> Ty -> Maybe (Exp EmptyEnv a)
upcast1 exp2 =
  case go exp2 of Sealed e -> safeCast e
  where

  e2d :: Exp2 -> Ty -> Dynamic
  e2d e2 ty =
    case e2 of
     T2 -> toDyn e2
     F2 -> toDyn e2
     If2 a b c ->
       let res :: Exp (env :: Env) res
           res = If undefined undefined undefined
       -- Managing environments gets very tricky here.
--           a1 :: Maybe (Exp env Bool)
--           a1 = fromDyn (e2d a BoolTy) unused
{-       
       case (fromDyn (e2d a BoolTy),
             fromDyn (e2d b ty),
             fromDyn (e2d c ty)) of
         _ -> undefined
-}
       in
--       toDyn res
       undefined
      
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
     Var2 x -> undefined $
       case upcastIdx x of
         _ -> undefined
     
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
      (If2 T2 (Var2 Zero2) (Lit2 4))

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
