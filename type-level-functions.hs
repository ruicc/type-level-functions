{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits
import Data.Type.Equality


type family Sum (ns :: [Nat]) :: Nat where
--    Sum ns = Foldl Add 0 ns
    Sum '[] = 0
    Sum (n ': ns) = n + Sum ns

type family Product (n :: [Nat]) :: Nat where
    Product '[] = 0
    Product (n ': ns) = n * Product ns

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

type family Filter (p :: a -> Bool) (as :: [a]) :: [a] where
    Filter p '[] = '[]
    Filter p (a ': as) = If (p a) (a ': Filter p as) (Filter p as)

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

type family Any (p :: a -> Bool) (as :: [a]) :: Bool where
     Any p '[] = False
     Any p (a ': as) = If (p a) True (Any p as)

type family Not (b :: Bool) :: Bool where
     Not True = False
     Not False = True

--type enumFromThenTo (a :: k) (b :: k) (c :: k) :: [k]
--type instance enumFromThenTo a b c

type family Fst (t :: (a, b)) :: a where
    Fst '(a, b) = a

type family Snd (t :: (a, b)) :: b where
    Snd '(a, b) = b

type family Odd (n :: Nat) :: Bool where
    Odd 0 = False
    Odd 1 = True
    Odd n = Odd (n - 2)

type family Even (n :: Nat) :: Bool where
    Even 0 = True
    Even 1 = False
    Even n = Even (n - 2)

type family Min (n :: Nat) (m :: Nat) :: Nat where
    Min n m = If (n <=? m) n m

type family Max (n :: Nat) (m :: Nat) :: Nat where
    Max n m = If (n <=? m) m n

type family Maybe_ (t :: b) (f :: a -> b) (ma :: Maybe a) :: b where
    Maybe_ b f ('Just a) = f a
    Maybe_ b f 'Nothing = b

type family Lookup (t :: a) (ls :: [(a, b)]) :: Maybe b where
    Lookup t '[] = 'Nothing
    Lookup t ('(a, b) ': ls) = If (a == b) ('Just b) (Lookup t ls)

type family (b1 :: Bool) || (b2 :: Bool) :: Bool where
    True || b2 = True
    False || b2 = b2

type family (b1 :: Bool) && (b2 :: Bool) :: Bool where
    True && b2 = b2
    False && b2 = False

type family Abs (n :: Nat) :: Nat where
    Abs n = If (n <=? 0) (Negate n) n

type family Negate (n :: Nat) :: Nat where
    Negate n = 0 - n

type family Succ (n :: Nat) :: Nat where
    Succ n = n + 1

type family Pred (n :: Nat) :: Nat where
    Pred n = n - 1

type family Head (as :: [a]) :: a where
    Head (a ': as) = a

type family Tail (as :: [a]) :: [a] where
    Tail (a ': as) = as

type family Last (as :: [a]) :: a where
    Last (a ': '[]) = a
    Last (a ': as) = Last as

type family Init (as :: [a]) :: [a] where
    Init (a ': '[]) = '[]
    Init (a ': as) = a ': Init as

type family Mod (n :: Nat) (m :: Nat) :: Nat where
    Mod n m =
        If ((0 <=? n) && (n <=? (m - 1)))
            n
            (Mod (n - m) m)

--------------------------------------------------------------------------------
type family (ma :: Maybe a) >>= (f :: a -> Maybe b) :: Maybe b where
    ('Just a) >>= f = f a
    'Nothing >>= f = 'Nothing

type family Return (e :: a) :: Maybe a where
    Return a = 'Just a

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

--------------------------------------------------------------------------------
-- Type level Fizzbuzz

data FB where
    N :: Nat -> FB
    S :: Symbol -> FB

type family Elm (n :: Nat) :: FB where
    Elm n =
        If (Mod n 15 == 0)
            (S "FizzBuzz")
            (If (Mod n 5 == 0)
                (S "Buzz")
                (If (Mod n 3 == 0)
                    (S "Fizz")
                    (N n)))

type family FizzBuzz (n :: Nat) :: [FB] where
    FizzBuzz n = FizzBuzz' [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]

type family FizzBuzz' (ns :: [Nat]) :: [FB] where
    FizzBuzz' '[] = '[]
    FizzBuzz' (n ': ns) = Elm n ': FizzBuzz' ns


main = putStrLn "OK!"
