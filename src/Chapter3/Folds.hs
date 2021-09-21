module Chapter3.Folds where

import Data.Bool (Bool(..), otherwise)
import Data.Either (Either(..))
import Data.Eq (Eq(..))
import Data.Function (id, (.))
import Data.Int (Int)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord((<)), min)
import Prelude (Show, undefined, error, Integral, Enum(..), (*), div, mod, (+), odd)

-- PAGE 42

data List a = Nil | Cons a (List a) deriving (Show)

wrap :: a -> List a
wrap x = Cons x Nil

nil :: List a -> Bool
nil Nil = True
nil _ = False

-- behaves like foldr from Prelude
foldL :: (a -> b -> b) -> b -> List a -> b
foldL _ e Nil = e
foldL f e (Cons x xs) = f x (foldL f e xs)


-- PAGE 43

-- h = foldL f e <=> h xs = case xs of
--                        Nil -> e
--                        Cons y ys -> f y (h ys)


-- Exercise 3.1
-- h . (foldL f e) == foldL f' e'
-- <=
-- (h (f a b) == f' a (h b)) && (h e == e')
--

-- Exercise 3.2

mapL :: (a -> b) -> List a -> List b
mapL _ Nil = Nil
mapL f (Cons x xs) = Cons (f x) (mapL f xs)

appendL :: List a -> List a -> List a
appendL Nil y = y
appendL x Nil = x
appendL (Cons x xs) y = Cons x (appendL xs y)

concatL :: List (List a) -> List a
concatL = foldL appendL Nil

--

-- Exercise 3.3
-- foldL f e . map g = foldL (f . g) e

--

-- PAGE 44

isort :: Ord a => List a -> List a
isort = foldL insert Nil
  where
    insert :: Ord a => a -> List a -> List a
    insert y Nil = wrap y
    insert y (Cons x xs)
      | y < x = Cons y (Cons x xs)
      | otherwise = Cons x (insert y xs)


-- Exercise 3.4
-- insert_1 y xs = (xs, insert y xs)

--

-- Exercise 3.5

paraL :: (a -> (List a, b) -> b) -> b -> List a -> b
paraL _ e Nil = e
paraL f e (Cons x xs) = f x (xs, paraL f e xs)

insert :: Ord a => a -> List a -> List a
insert = undefined

-- PAGE 45

unfoldL' :: (b -> Maybe (a, b)) -> b -> List a
unfoldL' f u = case f u of
                 Nothing -> Nil
                 Just (x, v) -> Cons x (unfoldL' f v)

unfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
unfoldL p f g b = if p b then Nil else Cons (f b) (unfoldL p f g (g b))

-- Exercise 3.6

-- unfoldL''' (express unfoldL' via unfoldL)
-- unfoldL''' :: (b -> Maybe (a, b)) -> b -> List a

-- unfoldL'' (express unfoldL via unfoldL)
-- unfoldL'' :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a

--

-- Exercise 3.7
-- unfoldL p f g . h = unfoldL p' f' g'
-- <=
-- (p . h == p') && (f . h == f') && (g . h == h . g')


--

foldL' :: (Maybe (a, b) -> b) -> List a -> b
foldL' f Nil = f Nothing
foldL' f (Cons x xs) = f (Just (x, foldL' f xs))

-- Exercise 3.8

-- foldL''' (express foldL' via fold)
-- foldL''' :: (Maybe (a, b) -> b) -> List a -> b

-- foldL'' (express foldL via foldL')
-- foldL'' :: (a -> b -> b) -> b -> List a -> b

--

-- PAGE 46

-- Exercise 3.9

foldLargs :: (a -> b -> b) -> b -> (Maybe (a,b) -> b)
foldLargs = undefined

unfoldLargs :: (b -> Bool) -> (b -> a) -> (b -> b) -> (b -> Maybe (a,b))
unfoldLargs = undefined

--

delmin' :: Ord a => List a -> Maybe (a, List a)
delmin' Nil = Nothing
delmin' xs = Just (y, deleteL y xs)
  where
    y = minimumL xs

minimumL :: Ord a => List a -> a
minimumL (Cons x xs) = foldL min x xs
minimumL Nil = error "Minumum is not defined on an empty list"

deleteL' :: Eq a => a -> List a -> List a
deleteL' _ Nil = Nil
deleteL' y (Cons x xs)
  | y == x = xs
  | otherwise = Cons x (deleteL' y xs)

ssort :: Ord a => List a -> List a
ssort = unfoldL' delmin

-- Exercise 3.10

deleteL :: Eq a => a -> List a -> List a
deleteL = deleteL'

--

-- Exercise 3.11

delmin :: Ord a => List a -> Maybe (a, List a)
delmin = delmin'

--

-- Page 47

bubble :: Ord a => List a -> Maybe (a, List a)
bubble = foldL step Nothing
  where
    step x Nothing = Just (x, Nil)
    step x (Just (y, ys))
      | x < y = Just (x, Cons y ys)
      | otherwise = Just (y, Cons x ys)

-- Exercise 3.12

bubble' :: Ord a => List a -> List a
bubble' = undefined

--

bsort :: Ord a => List a -> List a
bsort = unfoldL' bubble

-- Exercise 3.13

insert' :: Ord a => a -> List a -> List a
insert' = undefined

--

-- Exercise 3.14

apoL' :: (b -> Maybe (a, Either b (List a))) -> b -> List a
apoL' f u = case f u of
              Nothing -> Nil
              Just (x, Left v) -> Cons x (apoL' f v)
              Just (x, Right xs) -> Cons x xs

insert'' :: Ord a => a -> List a -> List a
insert'' = undefined

--

-- Page 48

fact :: (Integral a) => a -> a
fact = foldL (*) 1 . unfoldL (== 0) id pred


hyloL :: (a -> c -> c) -> c -> (b -> Bool) -> (b -> a) -> (b -> b) -> b -> c
hyloL f e p g h = foldL f e . unfoldL p g h


fact' :: (Integral a) => a -> a
fact' = hyloL (*) 1 (== 0) id pred

hyloL' :: (a -> c -> c) -> c -> (b -> Bool) -> (b -> a) -> (b -> b) -> b -> c
hyloL' f e p g h b = if p b then e else f (g b) (hyloL f e p g h (h b))

-- Exercise 3.15

translateDecimalStringToBoolList :: List Int -> List Bool
translateDecimalStringToBoolList = unfoldL (== 0) odd (`div` 2) . foldL (\ a b -> a + 10*b) 0

translateDecimalStringToBoolList' :: List Int -> List Bool
translateDecimalStringToBoolList' ints = bins
  where
    sum = foldL (\ a b -> a + 10*b) 0 ints
    bins = unfoldL (== 0) odd (`div` 2) sum

--

-- Page 49

data Nat = Zero | Suc Nat
