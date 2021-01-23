---
marp: true
footer: ''
---
<!-- 
theme: gaia
size: 16:9
paginate: true
-->
# 第9章　Greedy algorithms on graphs
全域木のうち，特別な形をした二つの問題を考える．
1. つながったグラフに対し最小コストで計算する問題
1. 有向グラフの計算問題（ある始点から他の全点に対する最小経路）

最短経路問題は，jogger問題（環状経路の最小和問題）を解くのに用いられてきた

---
### 9.1 Graphs and spanning trees (グラフと全域木)

有向グラフ/（無向）グラフは別種として考えた方が良い
* 最小コスト問題 -> 無向グラフ
* 最短経路問題 -> 有向グラフ

---
***有向グラフD***
$D=(V,E)$．(V：頂点集合，E：辺集合)
Eは$(u,v)$ (始点，終点)の集合で，$u\rightarrow v$ を向いている
有向グラフではループ$(u,u)$を持つことが可能．
 (Eは一つの辺を複数は含まない集合なので，各頂点につき最大一つ，始点と終点が同一になる)

→　n個の頂点を持つ有向グラフは，
$n^2$個を超える辺を持つことはなく，
ループがない場合には$n(n-1)$個を超える辺を持つことはない

---
***無向グラフG***
$G=(V,E)$
Eは$(u,v)$　（始点$\neq$終点），ループを持たない
$(u,v)=(v,u)$
n個の頂点を持つグラフは，$n(n-1)/2$個を超える辺をもたない

n個の頂点とe個の辺を持つ疎なグラフ，あるいは有向グラフでは，$e=O(n)$
密グラフ，あるいは有向グラフでは$e=O(n^2)$ ???


疎グラフ・蜜グラフそれぞれに適したアルゴリズムがある

---
章の目的のために，ラベル付されたグラフと有向グラフが必要．
ラベル付きグラフの各辺は重み（整数）でラベル付されている

```haskell
type Graph = ([Vertex],[Edge]) -- 数学的定義を反映
type Edge = (Vertex, Vertex, Weight)
type Vertex = Int
type Weight = Int
```

```haskell
nodes (vs,rs) = vs -- :: Graph -> [Vertex]
edges (vs,es) = es -- :: Graph -> [Edge]
source (u,v,w) = u -- :: Edge -> Vertex
target (u,v,w) = v -- :: Edge -> Vertex
weight (u,v,w) = w -- :: Edge -> Weight
```
---
グラフを，Vertex -> [(Vertex, Weight)]型の隣接関数(adjacentry func.)とみなすと，頂点が1..nの整数で名前付されていた場合，隣接関数のシンプルな実装は配列で書ける
```haskell
type AdjArray = Array Vertex [(Vertex, Weight)]
```

グラフの隣接配列表現は9.6でも登場します

（二つの表現の間の変換はExercise9.2　：　次頁）

---
## Exercise 9.2
頂点は1..nの整数値で名前付されていることを想定する．この時，次の関数を定義せよ
```haskell
toAdj :: Graph -> AdjArray
toGraph :: AdjArray -> Graph
```
A. 
```haskell
toAdj g = accumArray (flip (:)) [] (1,n) [u,(v,w) | (u,v,w) <- edges g]
toGraph arr = (indices arr, [(u,v,w) | 
                             (u,vws) <- assocs arr, (v,w) <- vws])
```
---
* 無向・有向グラフの経路は頂点$[v_0,v_1,\dots, v_k]$ の列で表現され，$(v_j, v_{j+1}) \scriptsize (0\le j \lt k)$は辺である．このような経路では，$v_k, v_0$を接続する．
* 環状グラフの経路は$[v_0,v_1,\dots, v_0]$ であるが，無向グラフでは$[v_0,v_1,v_0]$は環状ではない．$\because (v_0,v_1)=(v_1,v_0)$
(無向グラフでの環は3以上の頂点が必要)
* 環構造をもたない無向・有向グラフは，非環状グラフという
* 非環状グラフは，ある２頂点の間に少なくとも一つの経路を持つ
* 全頂点から，全頂点への経路が存在する時，接続されている
* 接続された非環状グラフは木，木の集合は森

---
```haskell
type Tree = Graph
type Forest = [Tree]
```

木は前章でのようなデータ型の意味ではなく，グラフの特別な一種のシノニム．
すべてのグラフは，接続された構成要素の集合に分解できる．グラフG=(V,E)の全域森は木の集合に分割できる
$$\scriptsize
e.g. (V_1,E_1),(V_2,E_2),\dots,(V_k,E_k) \\
V = \cup_{1\le i\le k}V_i, E = \cup_{1\le i\le k}E_i, 
$$
Gが接続されている場合，全域森は単一の全域木から成る．n個の頂点を持つ接続されたグラフの全域木は，n-1個の頂点を持つ．(なぜ？)

---
接続されたグラフGのMCST(全域木Tの最小コスト)は，Tの辺の重みの和を出来るだけ小さくすることに相当．
この節での狙いは，接続されたグラフのMCST計算の効率的方法を見つけること（より一般化された，接続されていないグラフについてはExercise9.5）

---
### 問題設定
* 町(頂点)と双方向に通れる道(辺)のネットワークから成る国を考える．
* 