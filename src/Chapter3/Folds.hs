module Chapter3.Folds where

import Prelude (Show)
import Data.Bool (Bool(..), otherwise)
import Data.Ord (Ord((<)))
import Data.Maybe (Maybe(..))

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


-- Exercise 3.3
-- foldL f e . map g = foldL (f . g) e

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


-- Exercise 3.5

paraL :: (a -> (List a, b) -> b) -> b -> List a -> b
paraL _ e Nil = e
paraL f e (Cons x xs) = f x (xs, paraL f e xs)


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


-- Exercise 3.7
-- unfoldL p f g . h = unfoldL p' f' g'
-- <=
-- (p . h == p') && (f . h == f') && (g . h == h . g')

foldL' :: (Maybe (a, b) -> b) -> List a -> b
foldL' f Nil = f Nothing
foldL' f (Cons x xs) = f (Just (x, foldL' f xs))

-- Exercise 3.8

-- foldL''' (express foldL' via fold)
-- foldL''' :: (Maybe (a, b) -> b) -> List a -> b

-- foldL'' (express foldL via foldL')
-- foldL'' :: (a -> b -> b) -> b -> List a -> b

-- PAGE 46

-- Exercise 3.9
