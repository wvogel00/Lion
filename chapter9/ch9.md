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
***問題設定 (p.221 Figure9.2のネットワーク)***
* 町(頂点)と双方向に通れる道(辺)のネットワークから成る国
* MCSTでの町間の経路は，最短経路である必要はない
(シュタイナー木ではMCSTは役に立たない：本の範疇を逸脱)

```haskell
mcst :: Graph -> Tree
mcst = minWith cost . (spats :: Graph -> [Tree])

cost :: Tree -> Int -- 木の辺の重みの和
cost = sum . map weight . edge
```
spats (全域木の略)は，グラフの全ての全域木を生成
$\iff$n頂点をもつグラフ(V,E)において，大きさn-1のEの全部分集合を見つける(非環状且つ接続されている)

---
***spatsの実装方法***
1. __Kruskal手法__ : 非環状であることを確認しつつ，空の状態からひとつずつ辺を追加し（森），最後の辺を追加する時に森は木に組み込まれる（接続が条件）
1. __Prim手法__ : 辺を一つ追加する度に非環状&&接続を確認（木）
  
Kruskal手法 $\longrightarrow$ 9.2
Prim手法 $\longrightarrow$ 9.4

---
### 9.2 Kruskal
木のリストではなく状態のリスト上で走ることを除けば，Huffmanアルゴリズムのmktreesに激似
初期状態 = 辺を持たない単一頂点の木の集合(森)と，全辺の集合
終了状態 = 1つの木(を含むリスト)と，未使用の辺集合

```haskell
type State = (Forest, [Edge])
spats :: Graph -> [Tree]
spats = map extract.until (all done) (concatMap steps).wrap.start

start :: Graph -> State
start g = ([([v],[]) | v <- nodes g], edges g)
```
---
```haskell
extract :: State -> Tree
extract ([t],_) = t
done :: State -> Bool
done = single.fst

steps :: State -> [State]
steps (ts,es) = [(add e ts, es') | (e,es') <- picks es, safeEdge e ts]
```
```extract``` ... 終了状態から全域木を取り出す
```steps``` ... 森に追加しても環状構造を生まない辺集合を選ぶ．エンドポイントが異なる木に属している場合は辺の追加が可能となり，二つの木を結合する

$$\scriptsize
picks :: [a] -> [(a,[a])] ... リスト中のある値xと，その補集合のリストを返す
$$

---
```haskell
safeEdge :: Edge -> Forest -> Bool
safeEdge e ts = find ts (source e) /= find ts (target e)

find :: Forest -> Vertex -> Tree --森のうち，頂点vを含む木を一つ返す
find ts v = head [t | t <- ts, any (==v) (nodes t)]

add :: Edge -> Forest -> Forest
add e ts = (nodes t1++nodes t2 , e:edges t1++edge t2) : rest
    where   t1 = find ts (source e)
            t2 = find ts (target e)
            rest = [t | t <- ts, t/= t1 and t/= t2]
```
```find``` ... 全頂点調べるため最悪計算量$\small=\Theta(n)$ (高効率の方法は後述)
```add``` ... 2つの木を結合し森に追加 (比較を除くと$\small O(n)$) (高効率は後述)

---
重み最小の辺を選ぶために，gstepを用いて8章での理論でマッピングされた経路を辿る．
```haskell
MCC = minWith cost.map extract.until (all done) (concatmap steps).wrap
```
前章より，
$\small
extract\ (until\ done\ gstep\ sx) \leftarrow MCC\ sx
$
全てのxsは，二つの条件を満足する
1. $\small
done\ sx \Rightarrow extract\ sx \leftarrow MCC\ sx$
$\small
extract\ ([t], es) = t \leftarrow MCC\ ([t],es)$
2. $\small
t \leftarrow MCC\ (gstep\ sx) \wedge t \leftarrow MCC\ sx \scriptsize\ (木tが存在する時の貪欲条件)$

---
辺のリストが重みについて昇順であることを仮定すると...
```haskell
gstep :: State -> State
gstep (ts, e:es) = if t1/=t2 then (ts', es) else gstep (ts,es)
    where t1 = find ts (source e)
          t2 = find ts (target e)
          ts' = (nodes t1++nodes t2 , e:edges t1++edge t2):rest
          rest = [t | t <- ts, t/= t1 and t/= t2] 
```
```gstep``` .. 異なる木t1,t2に端点がある辺を選択して木同士を結合する．

貪欲条件を検証してみる．
* sx=(ts,es) ... 森と未使用の辺集合
* e ... es中，最小で森によって安全な辺
---
t ← MCC sx
tがeを含んでいれば，tは常に最初にeによって結合可能 $\Rightarrow$ この式は貪欲条件を満たす
そうでない場合，tはeを含まずeの追加で環状構造を形成するので，
環状構造のいずれかの辺e'をeに置換する．
結果，$\small cost\ t' \le cost\ t$となる全域木t'が生まれる．$\tiny(\because weight\ e \le weight\ e')$

さらに言えば，t'はeを含むため最初にeを選択することで結合可能である，よって，tもt'も貪欲条件を満足する

```haskell
import Data.List (sortOn)
kruskal :: Graph -> Tree
kruskal = extract.until done gstep.start
start g = ([([v],[]) | v <- nodes g], sortOn weight (edges g))
```
---
別形式のKruskal
```haskell
kruskal :: Graph -> Tree
kruskal g = extract (apply (n-1) gstep (start g))
    where   n = length $ nodes g
```
頂点n個の接続されたグラフでは，gstepは丁度n-1回適用される

---
__時間見積もり__
* 頂点n個，辺e本．接続されたグラフ
* 任意の２頂点間に少なくとも一つの辺 $\small n-1\le e \le n(n-1)/2$
1. 辺のソート ... $\small O(e\log{e})$
1. find, add ... $\small O(n)$ステップ
1. 各find,add... 最悪，全辺調べるため$find..2e，add..n-1$回
1. 総時間計算量 ... $O(e\log{e}+en+n^2) = O(en)$

```find,add```がボトルネック．素集合データ構造使って高速化！
9.3へ急げ！

---
### 9.3 素集合データ構造
