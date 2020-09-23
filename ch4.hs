-- linear search
search0 f t = [x | x <- [0..t], t == f x]

type Nat = Int

-- binary search (ver 1)
search1 :: (Nat -> Nat) -> Nat -> [Nat]
search1 f t = seek (0, t)
    where seek (a,b) | a > b = []
                     | t < f m = seek (a,m-1)
                     | t == f m = [m]
                     | otherwise = seek (m+1,b)
                     where m = choose (a,b)
                           choose (a,b) = (a+b) `div` 2


-- binary search (ver 2)
search2 :: (Nat -> Nat) -> Nat -> [Nat]
search2 f t = if f x == t then [x] else []
    where x = smallest (bound f t) f t

bound :: (Nat -> Nat) -> Nat -> (Int,Nat)
bound f t = if t <= f 0 then (-1,0) else (div b 2, b)
    where b = until done (*2) 1
          done b = t <= f b
smallest (a,b) f t | a+1 == b = b
                   | t <= f m = smallest (a,m) f t
                   | otherwise = smallest (m,b) f t
                   where m = (a+b) `div` 2

