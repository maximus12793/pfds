{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Set

-- Proper spec.
data TreeSet a = Leaf | Node (TreeSet a) a (TreeSet a)

instance Ord a => Set TreeSet a where
  empty = Leaf
  insert x Leaf = Node Leaf x Leaf
  insert x (Node l y r)
    | x < y = Node (insert x l) y r
    | x > y = Node l y (insert x r)
    | otherwise = Node l y r
  member x Leaf = False
  member x (Node l y r)
    | x < y = member x l
    | x > y = member x r
    | otherwise = True
