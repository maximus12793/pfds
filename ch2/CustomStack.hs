module CustomStack (CustomStack) where
  data CustomStack a = NIL | Cons a (CustomStack a)

  empty :: CustomStack a
  empty = NIL

  isEmpty :: CustomStack a -> Bool
  isEmpty NIL = True
  isEmpty _ = False

  cons :: (a, CustomStack a) -> CustomStack a
  cons (x, s) = Cons x s

  head :: CustomStack a -> a
  head NIL = error "empty"
  head (Cons x _ ) = x

  tail :: CustomStack a -> CustomStack a
  tail NIL = error "empty"
  tail (Cons _ s) = s
