{- linear search -}

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

{- grid search -}
-- saddleback
sb_search f t = searchIn (0,t) -- サドルバック探索
    where searchIn (x,y) | y < 0  || t < x = []
                         | z < t = searchIn (x + 1,y)
                         | z == t = (x,y):searchIn (x + 1,y - 1)
                         | t < z = searchIn (x,y - 1)
                         where z = f (x,y)

-- 分割統治法
dc_search' :: ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
dc_search' f t = from (0,p) (q,0) where
    p = smallest (-1,t) (\y -> f (0,y)) t
    q = smallest (-1,t) (\x -> f (x,0)) t
    from (x1,y1) (x2,y2)
        | x2 < x1 || y1 < y2 = []
        | y1-y2 <= x1-x2 = row x
        | otherwise = col y
        where
        x = smallest (x1-1,x2) (\x -> f(x,r)) t
        y = smallest (y2-1,y1) (\y -> f(c,y)) t
        c = (x1+x2) `div` 2
        r = (y1+y2) `div` 2
        row x   |  z < t = from (x1,y1) (x2,r+1)
                | z == t = (x,r):from (x1,y1) (x-1,r+1)++from (x+1,r-1) (x2,y2)
                |  t < z = from (x1,y1) (x-1,r+1) ++ from (x,r-1) (x2,y2)
                where z = f (x,r)
        col y   |  z < t = from (c+1,y1) (x2,y2)
                | z == t = (c,y):from (x1,y1) (c-1,y+1)++from (c+1,y-1) (x2,y2)
                |  t < z = from (x1,y1) (c-1,y) ++ from (c+1,y-1) (x2,y2)
                where z = f (c,y)

gridf :: (Nat,Nat) -> Nat
gridf (x,y)
    | x > 11 || y > 13 = 1000
    | x < 0 || y < 0 = -1
    | otherwise = grid !! y !! x
    where
    grid = [ [100,101,112,124,176,212,257,316,452,472,487,497]
            ,[103,107,113,126,189,237,264,318,458,480,497,498]
            ,[116,128,131,134,237,240,267,346,469,481,515,523]
            ,[217,237,245,264,267,296,303,376,471,482,537,588]
            ,[272,245,283,296,299,302,313,441,523,529,587,589]
            ,[289,312,327,330,333,336,439,472,527,585,612,691]
            ,[312,313,363,366,411,472,523,601,612,647,698,704]
            ,[397,407,432,434,444,510,613,626,627,673,715,765]
            ,[403,411,441,444,547,583,653,656,679,691,765,768]
            ,[472,523,583,586,589,612,695,698,701,704,767,810]
            ,[475,597,627,630,633,717,739,742,845,848,851,894]
            ,[507,615,673,676,679,782,785,819,891,894,897,913]
            ,[519,621,752,797,801,827,833,865,917,924,945,998]
            ,[521,693,768,799,821,829,841,869,923,947,985,999]
            ]


------simpleな木--------------------------------------------
data Tree a = Nul | Nod (Tree a) a (Tree a)
    deriving Show

mktree' :: Ord a => [a] -> Tree a
mktree' [] = Nul
mktree' (x:xs) = Nod (mktree' ys) x (mktree' zs)
    where (ys,zs) = partition (<x) xs
partition p xs = (filter p xs, filter (not.p) xs)


------平衡木に対応--------------------------------------------
data BalancedTree a = Null | Node Nat (BalancedTree a) a (BalancedTree a)
    deriving Show

height :: BalancedTree a -> Nat
height Null = 0
height (Node h _ _ _) = h

node :: BalancedTree a -> a -> BalancedTree a -> BalancedTree a
node l x r = Node h l x r where h = 1 + max (height l) (height r)

mktree :: Ord a => [a] -> BalancedTree a
mktree = foldr insert Null

insert :: Ord a => a -> BalancedTree a -> BalancedTree a
insert x Null = node Null x Null
insert x (Node h l y r)
    | x < y = balance (insert x l) y r
    | x == y = Node h l y r
    | y < x = balance l y (insert x r)

bias :: BalancedTree a -> Int
bias (Node _ l x r) = height l - height r

balance :: BalancedTree a -> a -> BalancedTree a -> BalancedTree a
balance t1 x t2
    | abs (h1-h2) <= 1 = node t1 x t2
    | h1 == h2+2       = rotateR t1 x t2
    | h2 == h1+2       = rotateL t1 x t2
    where h1=height t1; h2 = height t2

rotateR t1 x t2 = if 0 <= bias t1 then rotr (node t1 x t2)
                                  else rotr (node (rotl t1) x t2)
rotateL t1 x t2 = if bias t2 <= 0 then rotl (node t1 x t2)
                                  else rotl (node t1 x (rotr t1))

rotr (Node _ (Node _ ll y rl) x r) = node ll y (node rl x r)
rotl (Node _ ll y (Node _ lrl z rrl)) = node (node ll y lrl) z rrl


-----gbalance--------------------------------------------
type Set a = BalancedTree a

balanceR :: Set a -> a -> Set a -> Set a
balanceR (Node _ l y r) x t2 = if height r >= height t2+2
    then balance l y (balanceR r x t2)
    else balance l y (node r x t2)

balanceL :: Set a -> a -> Set a -> Set a
balanceL (Node _ l y r) x t2 = if height l >= height t2+2
    then balance (balanceL l x t2) y r
    else balance (node l x t2) y r

gbalance :: Set a -> a -> Set a -> Set a
gbalance t1 x t2
    | abs (h1-h2) < 2 = balance  t1 x t2
    | h1 > h2+2       = balanceR t1 x t2
    | h1+2 < h2       = balanceL t1 x t2
    where h1 = height t1; h2 = height t2

-----data sets--------------------------------------------

member :: Ord a => a -> Set a -> Bool
member _ Null = False
member x (Node _ l y r) | x  < y    = member x l
                        | x  > y    = member x r
                        | otherwise = True

delete :: Ord a => a -> Set a -> Set a
delete _ Null = Null
delete x (Node _ l y r) | x < y     = balance (delete x l) y r
                        | x > y     = balance l y (delete x r)
                        | otherwise = combine l r

deleteMin :: Ord a => Set a -> (a,Set a)
deleteMin Null = undefined
deleteMin (Node _ Null x r) = (x,r)
deleteMin (Node _ l x r) = (y,balance t x r) where (y,t) = deleteMin l

combine :: Ord a => Set a -> Set a -> Set a
combine l Null = l
combine Null r = r
combine l r = balance l x t where (x,t) = deleteMin r

-------------------
-- 重複を許す木
mk_duplicateTree :: Ord a => [a] -> BalancedTree [a]
mk_duplicateTree = foldr insert_duplicate Null

insert_duplicate x Null = node Null [x] Null
insert_duplicate x (Node h l xs r)
    | x == head xs = Node h l (x:xs) r
    | x  < head xs = Node h (insert_duplicate x l) xs r
    | x  > head xs = Node h l xs (insert_duplicate x r)