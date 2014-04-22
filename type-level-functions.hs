{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits


type family Sum (ns :: [Nat]) :: Nat where
--    Sum ns = Foldl Add 0 ns
    Sum '[] = 0
    Sum (a ': as) = a + Sum as

type family Product (a :: [Nat]) :: Nat where
    Product '[] = 0
    Product (a ': as) = a * Product as

type family Concat (as :: [k]) (bs :: [k]) :: [k] where
    Concat a '[] = a
    Concat '[] b = b
    Concat (a ': as) bs = a ': Concat as bs

type family Zip (as :: [k]) (bs :: [l]) :: [(k, l)] where
    Zip a '[] = '[]
    Zip '[] b = '[]
    Zip (a ': as) (b ': bs) = '(a, b) ': Zip as bs

type family ZipWith (f :: a -> b -> c) (as :: [a]) (bs :: [b]) :: [c] where
    ZipWith f a '[] = '[]
    ZipWith f '[] b = '[]
    ZipWith f (a ': as) (b ': bs) = f a b ': ZipWith f as bs

type family Replicate (n :: Nat) (a :: k) :: [k] where
    Replicate 0 a = '[]
    Replicate n a = a ': Replicate (n - 1) a
 
type family Map (f :: a -> b) (as :: [a]) :: [b] where
    Map f '[] = '[]
    Map f (a ': as) = f a ': Map f as

type family Foldl (f :: b -> a -> b) (z :: b) (as :: [a]) :: b where
    Foldl f z '[] = '[]
    Foldl f z (a ': as) = Foldl f (f z a) as

type family Filter (f :: a -> Bool) (as :: [a]) :: [a] where
    Filter f '[] = '[]
    Filter f (a ': as) = If (f a) (a ': as) as

type family Length (as :: [a]) :: Nat where
    Length '[] = 0
    Length (a ': as) = 1 + Length as

type family Take (n :: Nat) (as :: [a]) :: [a] where
    Take n '[] = '[]
    Take 0 as = '[]
    Take n (a ': as) = a ': Take (n - 1) as

type family Reverse (as :: [a]) :: [a] where
    Reverse '[] = '[]
    Reverse as = Rev as '[]

type family Rev (as :: [a]) (bs :: [a]) :: [a] where
    Rev '[] k = k
    Rev (a ': as) k = Rev as (a ': k)

type family Null (as :: [a]) :: Bool where
    Null '[] = True
    Null as = False

type family Or (bs :: [Bool]) :: Bool where
     Or '[] = False
     Or (False ': bs) = Or bs
     Or (True ': bs) = True

type family And (bs :: [Bool]) :: Bool where
     And '[] = True
     And (True ': bs) = And bs
     And (False ': bs) = False

type family Not (b :: Bool) :: Bool where
     Not True = False
     Not False = True

--------------------------------------------------------------------------------
type family Const (a :: k) (b :: l) :: k where
    Const a b = a

type family Id (a :: k) :: k where
    Id a = a

type family If (p :: Bool) (a :: k) (b :: k) :: k where
    If True a b = a
    If False a b = b

type family Add (a :: Nat) (b :: Nat) :: Nat where
    Add a b = a + b

type family Add1 (a :: Nat) :: Nat where
    Add1 a = a + 1

--------------------------------------------------------------------------------
type Con = Concat (1 ': [2, 4, 8]) [Add1 (Add1 1), 3, 5]

a :: Const Int (Sum Con)
a = 3

main = putStrLn "OK!"
