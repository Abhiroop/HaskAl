{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module FingerTree where

import Data.Monoid

data Tree v a = Leaf v a | Branch v (Tree v a) (Tree v a)

newtype Size a = Size {getSize :: Int}

-------------List with random acces----------------

toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y

leaf :: a -> Tree (Size a) a
leaf a = Leaf (Size 1) a

-- branch :: Tree (Size a) a -> Tree (Size a) a -> Tree (Size a) a
-- branch x y = Branch (Size (tag x + tag y)) x y

-- tag :: Tree (Size a) a -> Int
-- tag (Leaf v _)     = getSize v
-- tag (Branch v _ _) = getSize v

head' :: Tree v a -> a
head' (Leaf _ a) = a
head' (Branch _ x _) = head' x

(!!!) :: Tag (Size a) => Tree (Size a) a-> Int -> a
(Leaf _ a)     !!! 0 = a
(Branch _ x y) !!! n
  | n < getSize (tag x) = x !!! n
  | otherwise           = y !!! (n - getSize (tag x)) 

instance Monoid (Size a)  where
  mempty  = Size 0
  Size x `mappend` Size y = Size (x + y)

-----------Priority Queue with O(log n) lookup
newtype Priority a = Priority {getPriority :: Int}

-- tag' :: Tree (Priority a) a -> Int
-- tag' (Leaf v _)     = getPriority v
-- tag' (Branch v _ _) = getPriority v

winner :: Tag (Priority a) => Tree (Priority a) a -> a
winner t = go t
   where
   go (Leaf _ a) = a
   go (Branch _ x y)
     | getPriority (tag x) == getPriority (tag t) = go x -- winner on left
     | getPriority (tag y) == getPriority (tag t) = go y -- winner on right3


instance Monoid (Priority a) where
  mempty  =  Priority (maxBound :: Int)
  Priority x `mappend` Priority y = Priority (min x y)


class Tag t where
  tag :: Tree t a -> t

instance Tag (Tree (Size a) a) where
  tag (Leaf v _)     = v
  tag (Branch v _ _) = v

instance Tag (Tree (Priority a) a) where
  tag (Leaf v _)     = v
  tag (Branch v _ _) = v


branch :: (Tag v, Monoid v) => Tree v a -> Tree v a -> Tree v a
branch x y = Branch (tag x <> tag y) x y

