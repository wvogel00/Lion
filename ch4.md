---
marp: true
footer: '19/10/11'
---
<!-- 
theme: gaia
size: 16:9
paginate: true
-->
# 第四章　Binary search (二分探索)
#### 分割統治法の最もシンプルな一例
* 元データを二分し，二つの部分問題へ分割
* 二分探索の特徴は，部分問題がトリビアルであること

二つの例をみた後，二分探索をデータ構造（二分探索木）にカプセル化する

---
# 4-1.1 一次元探索問題
#### 自然数の狭義単調増加関数 $f (x < y \Rightarrow f x < f y)$
```
t = f x
```
となるxを探すことを考えると...
```haskell
search :: (Nat -> Nat>) -> Nat -> [Nat] -- 線形探索
search f t = [x | x <- [0..t], t == f x]
```
単一要素リストあるいは空リストを得る．

---
# 4-1.2 一次元探索問題
```haskell
search f t = seek (0,t) where
    seek (a,b) = [x | x <- [a,b], t == f x]
```
seekを書き下す
```haskell
seek (a,b) = [x | x <- [a .. m-1], t == f x] -- t < f m の時のみ
          ++ [m | t == f m]
          ++ [x | x <- [m+1 .. b] == f x] -- t > f m の時のみ
```

値が片側のリストにしか存在しない性質を利用する．

---
# 4-1.3 一次元探索問題
```haskell
search :: (Nat -> Nat) -> Nat -> [Nat]
search f t = seek (0,t)
    where seek (a,b) | a > b = []
                     | t < f m = seek (a,m-1)
                     | t == f m = [m]
                     | otherwise = seek (m+1,b)
                     where m = choose (a,b)
```

---
# 4-2 二次元探索問題
### the content
---
# 4-3 二分探索木
---
# 4-4 動的セット（Dynamic set）
---
# Excercise