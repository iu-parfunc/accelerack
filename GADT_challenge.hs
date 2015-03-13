{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

import Data.Dynamic

--------------------------------------------------------------------------------
-- (1) GADT based AST 

data Env = Empty | forall a . Extend a Env
 deriving Typeable

data Exp (env :: Env) (a :: *) where
  T   :: Exp env Bool
  F   :: Exp env Bool
  If  :: Exp env Bool -> Exp env a -> Exp env a -> Exp env a
  Lit :: Int -> Exp env Int
  Add :: Exp env Int -> Exp env Int -> Exp env Int 
  Let :: Exp env t1
      -> Exp (Extend t1 env) a
      -> Exp env a  
  Var :: Idx env a -> Exp env a
 deriving Typeable

data Idx (env :: Env) t where
  Zero ::              Idx (Extend t env) t
  Succ :: Idx env t -> Idx (Extend s env) t

deriving instance (Show (Exp env a))

deriving instance (Show (Idx env t))

--------------------------------------------------------------------------------
-- (2) ADT version:

data Exp2 = T2 | F2
          | If2 Exp2 Exp2 Exp2
          | Lit2 Int
          | Add2 Exp2 Exp2 
          | Let2 Exp2 Exp2
          | Var2 Idx2
  deriving (Show, Typeable)

data Idx2 = Zero2 | Succ2 Idx2
  deriving (Show, Typeable)

data Ty = BoolTy | IntTy | AnyTy

--------------------------------------------------------------------------------

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
      let loop :: Idx env t -> Idx2
          loop Zero = Zero2
          loop (Succ idx) = Succ2 (loop idx)
      in Var2 $ loop ix 

--------------------------------------------------------------------------------
-- Option 1: the old way.  Sealed, monomorphic data and Data.Dynamic.
    
data Sealed = forall env a . Sealed (Exp env a) -- Dynamic

-- upcast :: Exp2 -> Ty -> Exp env a
upcast :: Exp2 -> Ty -> Sealed
upcast e2 ty =
  case e2 of
   T2 -> Sealed T -- (toDyn T)
   F2 -> Sealed F 
   (If2 x1 x2 x3) ->
     case (upcast x1 BoolTy,
           upcast x2 ty,
           upcast x3 ty) of
       (Sealed e, _, _) ->
--         fromDynamic
--       Sealed $ If e undefined undefined
         undefined
   (Lit2 x) -> undefined
   (Add2 x1 x2) -> undefined
   (Let2 x1 x2) -> undefined
   (Var2 x) -> undefined

--------------------------------------------------------------------------------
-- Option 2: the new way.  Consumer demands the type and we downcast
-- without ever sealing.

-- FINISHME   

--------------------------------------------------------------------------------
-- Test programs:

p1a :: Exp env Int
p1a = Let (Lit 5) 
      (If T (Var Zero) (Lit 4))

p1b :: Exp2
p1b = Let2 (Lit2 5) 
      (If2 T2 (Var2 Zero2) (Lit2 4))

--------------------------------------------------------------------------------

main :: IO ()
main = do print p1a
          print p1b
          print (downcast p1a)
