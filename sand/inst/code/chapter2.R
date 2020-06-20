# SAND with R, chapter2.tex

# CHUNK 1
library(igraph)
g <- graph_from_literal(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 
                    4-6, 4-7, 5-6, 6-7)

# CHUNK 2
V(g)
# ---
## + 7/7 vertices, named, from fac8b33:
## [1] 1 2 3 4 5 6 7
# ---

# CHUNK 3
E(g)
# ---
## + 10/10 edges from fac8b33 (vertex names):
## [1] 1--2 1--3 2--3 2--4 3--5 4--5 4--6 4--7 5--6 6--7
# ---

# CHUNK 4
print_all(g)
# ---
## IGRAPH fac8b33 UN-- 7 10 -- 
## + attr: name (v/c)
## + edges (vertex names):
## 1 -- 2, 3
## 2 -- 1, 3, 4
## 3 -- 1, 2, 5
## 4 -- 2, 5, 6, 7
## 5 -- 3, 4, 6
## 6 -- 4, 5, 7
## 7 -- 4, 6
# ---

# CHUNK 5
plot(g)

# CHUNK 6
dg <- graph_from_literal(1-+2, 1-+3, 2++3)
plot(dg)

# CHUNK 7
dg <- graph_from_literal(Sam-+Mary, Sam-+Tom, 
                          Mary++Tom)
print_all(dg)
# ---
## IGRAPH a21f0d9 DN-- 3 4 -- 
## + attr: name (v/c)
## + edges from a21f0d9 (vertex names):
## [1] Sam ->Mary Sam ->Tom  Mary->Tom  Tom ->Mary
# ---

# CHUNK 8
V(dg)$name <- c("Sam", "Mary", "Tom")

# CHUNK 9
E(dg)
# ---
## + 4/4 edges from 062bf79 (vertex names):
## [1] Sam ->Mary Sam ->Tom  Mary->Tom  Tom ->Mary
# ---

# CHUNK 10
as_adjacency_matrix(g)
# ---
## 7 x 7 sparse Matrix of class "dgCMatrix"
##   1 2 3 4 5 6 7
## 1 . 1 1 . . . .
## 2 1 . 1 1 . . .
## 3 1 1 . . 1 . .
## 4 . 1 . . 1 1 1
## 5 . . 1 1 . 1 .
## 6 . . . 1 1 . 1
## 7 . . . 1 . 1 .
# ---

# CHUNK 11
h <- induced_subgraph(g, 1:5)
print_all(h)
# ---
## IGRAPH 2560ed9 UN-- 5 6 -- 
## + attr: name (v/c)
## + edges from 2560ed9 (vertex names):
## [1] 1--2 1--3 2--3 2--4 3--5 4--5
# ---

# CHUNK 12
h <- g - vertices(c(6,7))

# CHUNK 13
h <- h + vertices(c(6,7))
g <- h + edges(c(4,6),c(4,7),c(5,6),c(6,7))

# CHUNK 14
h1 <- h
h2 <- graph_from_literal(4-6, 4-7, 5-6, 6-7)
g <- union(h1,h2)

# CHUNK 15
V(dg)$name
# ---
## [1] "Sam"  "Mary" "Tom"
# ---

# CHUNK 16
V(dg)$gender <- c("M","F","M")

# CHUNK 17
V(g)$color <- "red"

# CHUNK 18
is_weighted(g)
# ---
## [1] FALSE
# ---
wg <- g
E(wg)$weight <- runif(ecount(wg))
is_weighted(wg)
# ---
## [1] TRUE
# ---

# CHUNK 19
g$name <- "Toy Graph"

# CHUNK 20
library(sand)
g.lazega <- graph_from_data_frame(elist.lazega,
                                   directed="FALSE",
                                   vertices=v.attr.lazega)
g.lazega$name <- "Lazega Lawyers"

# CHUNK 21
vcount(g.lazega)
# ---
## [1] 36
# ---

# CHUNK 22
ecount(g.lazega)
# ---
## [1] 115
# ---

# CHUNK 23
vertex_attr_names(g.lazega)
# ---
## [1] "name"      "Seniority" "Status"    "Gender"   
## [5] "Office"    "Years"     "Age"       "Practice" 
## [9] "School"
# ---

# CHUNK 24
is_simple(g)
# ---
## [1] TRUE
# ---

# CHUNK 25
mg <- g + edge(2,3)
print_all(mg)
# ---
## IGRAPH f00a980 UN-- 7 11 -- Toy Graph
## + attr: name (g/c), name (v/c), color (v/c)
## + edges (vertex names):
## 1 -- 2, 3
## 2 -- 1, 3, 3, 4
## 3 -- 1, 2, 2, 5
## 4 -- 2, 5, 6, 7
## 5 -- 3, 4, 6
## 6 -- 4, 5, 7
## 7 -- 4, 6
# ---
is_simple(mg)
# ---
## [1] FALSE
# ---

# CHUNK 26
E(mg)$weight <- 1
wg2 <- simplify(mg)
is_simple(wg2)
# ---
## [1] TRUE
# ---

# CHUNK 27
print_all(wg2)
# ---
## IGRAPH 78518b0 UNW- 7 10 -- Toy Graph
## + attr: name (g/c), name (v/c), color (v/c), weight
## | (e/n)
## + edges (vertex names):
## 1 -- 2, 3
## 2 -- 1, 3, 4
## 3 -- 1, 2, 5
## 4 -- 2, 5, 6, 7
## 5 -- 3, 4, 6
## 6 -- 4, 5, 7
## 7 -- 4, 6
# ---

# CHUNK 28
E(wg2)$weight
# ---
## [1] 1 1 2 1 1 1 1 1 1 1
# ---

# CHUNK 29
neighbors(g,5)
# ---
## + 3/7 vertices, named, from 2e0a0da:
## [1] 3 4 6
# ---

# CHUNK 30
degree(g)
# ---
## 1 2 3 4 5 6 7
## 2 3 3 4 3 3 2
# ---

# CHUNK 31
degree(dg, mode="in")
# ---
##  Sam Mary  Tom
##    0    2    2
# ---
degree(dg, mode="out")
# ---
##  Sam Mary  Tom
##    2    1    1
# ---

# CHUNK 32
is_connected(g)
# ---
## [1] TRUE
# ---

# CHUNK 33
clusters(g)
# ---
## $`membership`
## 1 2 3 4 5 6 7 
## 1 1 1 1 1 1 1 
##
## $csize
## [1] 7
##
## $no
## [1] 1
## 
# ---

# CHUNK 34
is_connected(dg,mode="weak")
# ---
## [1] TRUE
# ---
is_connected(dg,mode="strong")
# ---
## [1] FALSE
# ---

# CHUNK 35
diameter(g, weights=NA)
# ---
## [1] 3
# ---

# CHUNK 36
g.full <- make_full_graph(7)
g.ring <- make_ring(7)
g.tree <- make_tree(7, children=2, mode="undirected")
g.star <- make_star(7, mode="undirected")
par(mfrow=c(2, 2), mai = c(0.2, 0.2, 0.2, 0.2))
plot(g.full)
plot(g.ring)
plot(g.tree)
plot(g.star)

# CHUNK 37
is_dag(dg)
# ---
## [1] FALSE
# ---

# CHUNK 38
g.bip <- graph_from_literal(actor1:actor2:actor3,
   movie1:movie2, actor1:actor2 - movie1,
   actor2:actor3 - movie2)
V(g.bip)$type <- grepl("^movie", V(g.bip)$name)
print_all(g.bip, v=T)
# ---
## IGRAPH 68780ab UN-B 5 4 -- 
## + attr: name (v/c), type (v/l)
## + vertex attributes:
## |       name  type
## | [1] actor1 FALSE
## | [2] actor2 FALSE
## | [3] actor3 FALSE
## | [4] movie1  TRUE
## | [5] movie2  TRUE
## + edges from 68780ab (vertex names):
## [1] actor1--movie1 actor2--movie1 actor2--movie2
## [4] actor3--movie2
# ---

# CHUNK 39
proj <- bipartite_projection(g.bip)
print_all(proj[[1]])
# ---
## IGRAPH 3782b1c UNW- 3 2 -- 
## + attr: name (v/c), weight (e/n)
## + edges from 3782b1c (vertex names):
## [1] actor1--actor2 actor2--actor3
# ---
print_all(proj[[2]])
# ---
## IGRAPH 3782b1c UNW- 2 1 -- 
## + attr: name (v/c), weight (e/n)
## + edge from 3782b1c (vertex names):
## [1] movie1--movie2
# ---

