{-# LANGUAGE MultiParamTypeClasses #-}

module Set(Set(..)) where
  -- assumes multi-param type class
  class Set s a where
    empty :: s a
    insert :: a -> s a -> s a
    member :: a -> s a -> Bool

