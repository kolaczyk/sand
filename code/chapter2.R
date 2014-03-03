# SAND with R, chapter2.tex

# CHUNK 1
library(igraph)
g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6,
                    4-7, 5-6, 6-7)

# CHUNK 2
V(g)
# ---
## Vertex sequence:
## [1] "1" "2" "3" "4" "5" "6" "7"
# ---

# CHUNK 3
E(g)
# ---
## Edge sequence:
##            
## [1]  2 -- 1
## [2]  3 -- 1
## [3]  3 -- 2
## [4]  4 -- 2
## [5]  5 -- 3
## [6]  5 -- 4
## [7]  6 -- 4
## [8]  7 -- 4
## [9]  6 -- 5
## [10] 7 -- 6
# ---

# CHUNK 4
str(g)
# ---
## IGRAPH UN-- 7 10 -- 
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
dg <- graph.formula(1-+2, 1-+3, 2++3)
plot(dg)

# CHUNK 7
dg <- graph.formula(Sam-+Mary, Sam-+Tom, Mary++Tom)
str(dg)
# ---
## IGRAPH DN-- 3 4 -- 
## + attr: name (v/c)
## + edges (vertex names):
## [1] Sam ->Mary Sam ->Tom  Mary->Tom  Tom ->Mary
# ---

# CHUNK 8
V(dg)$name <- c("Sam", "Mary", "Tom")

# CHUNK 9
E(dg)
# ---
## Edge sequence:
##                 
## [1] Sam  -> Mary
## [2] Sam  -> Tom 
## [3] Mary -> Tom 
## [4] Tom  -> Mary
# ---

# CHUNK 10
get.adjacency(g)
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
h <- induced.subgraph(g, 1:5)
str(h)
# ---
## IGRAPH UN-- 5 6 -- 
## + attr: name (v/c)
## + edges (vertex names):
## [1] 1--2 1--3 2--3 2--4 3--5 4--5
# ---

# CHUNK 12
h <- g - vertices(c(6,7))

# CHUNK 13
h <- h + vertices(c(6,7))
g <- h + edges(c(4,6),c(4,7),c(5,6),c(6,7))

# CHUNK 14
h1 <- h
h2 <- graph.formula(4-6, 4-7, 5-6, 6-7)
g <- graph.union(h1,h2)

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
is.weighted(g)
# ---
## [1] FALSE
# ---
wg <- g
E(wg)$weight <- runif(ecount(wg))
is.weighted(wg)
# ---
## [1] TRUE
# ---

# CHUNK 19
g$name <- "Toy Graph"

# CHUNK 20
library(sand)
g.lazega <- graph.data.frame(elist.lazega, 
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
list.vertex.attributes(g.lazega)
# ---
## [1] "name"      "Seniority" "Status"    "Gender"   
## [5] "Office"    "Years"     "Age"       "Practice" 
## [9] "School"
# ---

# CHUNK 24
is.simple(g)
# ---
## [1] TRUE
# ---

# CHUNK 25
mg <- g + edge(2,3)
str(mg)
# ---
## IGRAPH UN-- 7 11 -- Toy Graph
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
is.simple(mg)
# ---
## [1] FALSE
# ---

# CHUNK 26
E(mg)$weight <- 1
wg2 <- simplify(mg)
is.simple(wg2)
# ---
## [1] TRUE
# ---

# CHUNK 27
str(wg2)
# ---
## IGRAPH UNW- 7 10 -- Toy Graph
## + attr: name (g/c), name (v/c), color (v/c),
##   weight (e/n)
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
##  [1] 1 1 2 1 1 1 1 1 1 1
# ---

# CHUNK 29
neighbors(g,5)
# ---
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
is.connected(g)
# ---
## [1] TRUE
# ---

# CHUNK 33
clusters(g)
# ---
## $membership
## [1] 1 1 1 1 1 1 1
## 
## $csize
## [1] 7
## 
## $no
## [1] 1
# ---

# CHUNK 34
is.connected(dg, mode="weak")
# ---
## [1] TRUE
# ---
is.connected(dg, mode="strong")
# ---
## [1] FALSE
# ---

# CHUNK 35
diameter(g, weights=NA)
# ---
## [1] 3
# ---

# CHUNK 36
g.full <- graph.full(7)
g.ring <- graph.ring(7)
g.tree <- graph.tree(7, children=2, mode="undirected")
g.star <- graph.star(7, mode="undirected")
par(mfrow=c(2, 2))
plot(g.full)
plot(g.ring)
plot(g.tree)
plot(g.star)

# CHUNK 37
is.dag(dg)
# ---
## [1] FALSE
# ---

# CHUNK 38
g.bip <- graph.formula(actor1:actor2:actor3,
   movie1:movie2, actor1:actor2 - movie1,
   actor2:actor3 - movie2)
V(g.bip)$type <- grepl("^movie", V(g.bip)$name)
str(g.bip, v=T)
# ---
## IGRAPH UN-B 5 4 -- 
## + attr: name (v/c), type (v/l)
## + vertex attributes:
##       name  type
## [1] actor1 FALSE
## [2] actor2 FALSE
## [3] actor3 FALSE
## [4] movie1  TRUE
## [5] movie2  TRUE
## + edges (vertex names):
## [1] actor1--movie1 actor2--movie1 actor2--movie2
## [4] actor3--movie2
# ---

# CHUNK 39
proj <- bipartite.projection(g.bip)
str(proj[[1]])
# ---
## IGRAPH UNW- 3 2 -- 
## + attr: name (v/c), weight (e/n)
## + edges (vertex names):
## [1] actor1--actor2 actor2--actor3
# ---
str(proj[[2]])
# ---
## IGRAPH UNW- 2 1 -- 
## + attr: name (v/c), weight (e/n)
## + edges (vertex names):
## [1] movie1--movie2
# ---

