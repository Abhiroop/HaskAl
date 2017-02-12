{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module FingerTree where

import Data.Monoid

data Tree v a = Leaf v a | Branch v (Tree v a) (Tree v a)

newtype Size = Size {getSize :: Int}

-------------List with random acces----------------

toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y

-- leaf :: a -> Tree Size a
-- leaf a = Leaf (Size 1) a

-- branch :: Tree Size a -> Tree Size a -> Tree Size a
-- branch x y = Branch (Size (tag x + tag y)) x y

-- tag :: Tree Size a -> Int
-- tag (Leaf v _)     = getSize v
-- tag (Branch v _ _) = getSize v

head' :: Tree v a -> a
head' (Leaf _ a) = a
head' (Branch _ x _) = head' x

(!!!) :: Tag Size => Tree Size a-> Int -> a
(Leaf _ a)     !!! 0 = a
(Branch _ x y) !!! n
  | n < getSize (tag x) = x !!! n
  | otherwise           = y !!! (n - getSize (tag x)) 

instance Monoid Size  where
  mempty  = Size 0
  Size x `mappend` Size y = Size (x + y)

-----------Priority Queue with O(log n) lookup
newtype Priority = Priority {getPriority :: Int}

-- tag' :: Tree Priority a -> Int
-- tag' (Leaf v _)     = getPriority v
-- tag' (Branch v _ _) = getPriority v

winner :: Tag Priority => Tree Priority a -> a
winner t = go t
   where
   go (Leaf _ a) = a
   go (Branch _ x y)
     | getPriority (tag x) == getPriority (tag t) = go x -- winner on left
     | getPriority (tag y) == getPriority (tag t) = go y -- winner on right3

instance Monoid Priority where
  mempty  =  Priority (maxBound :: Int)
  Priority x `mappend` Priority y = Priority (min x y)

class Tag t where
  tag :: Tree t a -> t

instance Tag (Tree Size a) where
  tag (Leaf v _)     = v
  tag (Branch v _ _) = v

instance Tag (Tree Priority a) where
  tag (Leaf v _)     = v
  tag (Branch v _ _) = v

branch :: (Tag v, Monoid v) => Tree v a -> Tree v a -> Tree v a
branch x y = Branch (tag x <> tag y) x y


-- class Monoid v => Measured v a where
--     measure :: a -> v

-- leaf :: a -> b -> Tree v a
-- leaf (Leaf v a) = Leaf v a v= 

-- instance Measured Size Int where
--     measure _ = Size 1            -- one element = size 1

-- instance Measured Priority Int where
--     measure a = Priority a   -- urgency of the element

-- instance (Tag v, Measured v a) => Measured v (Tree v a) where
--     measure = tag

----------------------------O(logn) Search---------------------------
search :: (Monoid v, Tag v) => (v -> Bool) -> Tree v a -> Maybe a
search p t
    | p (tag t) = Just (go mempty p t)
    | otherwise     = Nothing
    where
    go i p (Leaf _ a) = a
    go i p (Branch _ l r)
        | p (i <> tag l) = go i p l
        | otherwise          = go (i <> tag l) p r


