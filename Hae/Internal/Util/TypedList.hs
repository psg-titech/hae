{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module Hae.Internal.Util.TypedList where

data TList :: (* -> *) -> * -> * where
  TNil :: TList f ()
  (:::) :: f t -> TList f ts -> TList f (t, ts)

newtype I a = I
  { unI :: a
  }

newtype K b a = K
  { unK :: b
  }

thead :: TList f (t, ts) -> f t
thead (x ::: xs) = x

ttail :: TList f (t, ts) -> TList f ts
ttail (x ::: xs) = xs

tlength :: TList v t -> Int
tlength TNil     = 0
tlength (x:::xs) = 1 + tlength xs

tmap :: (forall t. f t -> g t) -> TList f t -> TList g t
tmap h TNil       = TNil
tmap h (x ::: xs) = h x ::: tmap h xs

tmapK :: (a -> b) -> TList (K a) ts -> TList (K b) ts
tmapK f = tmap (K . f . unK)

tzipWith :: (forall t. f t -> g t -> h t) ->
            TList f ts -> TList g ts -> TList h ts
tzipWith f TNil TNil             = TNil
tzipWith f (x ::: xs) (y ::: ys) = f x y ::: tzipWith f xs ys

data RList :: * -> * where
  RNil :: RList ()
  RCons :: RList ts -> RList (t, ts)

class CList t where
  cList :: RList t

instance CList () where
  cList = RNil

instance CList ts => CList (t, ts) where
  cList = RCons cList

titerate' :: RList ts -> (a -> a) -> a -> TList (K a) ts
titerate' RNil f n       = TNil
titerate' (RCons xs) f n = K n ::: titerate' xs f (f n)

titerate :: CList ts => (a -> a) -> a -> TList (K a) ts
titerate = titerate' cList

tenumFrom :: CList ts => Int -> TList (K Int) ts
tenumFrom n = titerate (+1) n

ttoList :: TList (K a) ts -> [a]
ttoList TNil         = []
ttoList (K x ::: xs) = x : ttoList xs
