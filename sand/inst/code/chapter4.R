# SAND with R, chapter4.tex

# CHUNK 1
library(sand)
data(karate)
hist(degree(karate), col="lightblue", xlim=c(0,50),
   xlab="Vertex Degree", ylab="Frequency", main="")

# CHUNK 2
hist(graph.strength(karate), col="pink",
   xlab="Vertex Strength", ylab="Frequency", main="")

# CHUNK 3
library(igraphdata)
data(yeast)

# CHUNK 4
ecount(yeast)
# ---
## [1] 11855
# ---

# CHUNK 5
vcount(yeast)
# ---
## [1] 2617
# ---

# CHUNK 6
d.yeast <- degree(yeast)
hist(d.yeast,col="blue",
   xlab="Degree", ylab="Frequency",
   main="Degree Distribution")

# CHUNK 7
dd.yeast <- degree.distribution(yeast)
d <- 1:max(d.yeast)-1
ind <- (dd.yeast != 0)
plot(d[ind], dd.yeast[ind], log="xy", col="blue",
   xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
   main="Log-Log Degree Distribution")

# CHUNK 8
a.nn.deg.yeast <- graph.knn(yeast,V(yeast))$knn
plot(d.yeast, a.nn.deg.yeast, log="xy", 
   col="goldenrod", xlab=c("Log Vertex Degree"),
   ylab=c("Log Average Neighbor Degree"))

# CHUNK 9
A <- get.adjacency(karate, sparse=FALSE)
library(network)
g <- network::as.network.matrix(A)
library(sna)
sna::gplot.target(g, degree(g), main="Degree",
   circ.lab = FALSE, circ.col="skyblue",
   usearrows = FALSE,
   vertex.col=c("blue", rep("red", 32), "yellow"),
   edge.col="darkgray")

# CHUNK 10
l <- layout.kamada.kawai(aidsblog)
plot(aidsblog, layout=l, main="Hubs", vertex.label="",
   vertex.size=10 * sqrt(hub.score(aidsblog)$vector))
plot(aidsblog, layout=l, main="Authorities", 
   vertex.label="", vertex.size=10 * 
   sqrt(authority.score(aidsblog)$vector))

# CHUNK 11
eb <- edge.betweenness(karate)
E(karate)[order(eb, decreasing=T)[1:3]]
# ---
## Edge sequence:
##                          
## [53] John A   -- Actor 20
## [14] Actor 20 -- Mr Hi   
## [16] Actor 32 -- Mr Hi
# ---

# CHUNK 12
table(sapply(cliques(karate), length))
# ---
## 
##  1  2  3  4  5 
## 34 78 45 11  2
# ---

# CHUNK 13
cliques(karate)[sapply(cliques(karate), length) == 5]
# ---
## [[1]]
## [1] 1 2 3 4 8
## 
## [[2]]
## [1]  1  2  3  4 14
# ---

# CHUNK 14
table(sapply(maximal.cliques(karate), length))
# ---
## 
##  2  3  4  5 
## 11 21  2  2
# ---

# CHUNK 15
clique.number(yeast)
# ---
## [1] 23
# ---

# CHUNK 16
cores <- graph.coreness(karate)
sna::gplot.target(g, cores, circ.lab = FALSE, 
              circ.col="skyblue", usearrows = FALSE, 
              vertex.col=cores, edge.col="darkgray")
detach("package:network")
detach("package:sna")

# CHUNK 17
aidsblog <- simplify(aidsblog)
dyad.census(aidsblog)
# ---
## $mut
## [1] 3
## 
## $asym
## [1] 177
## 
## $null
## [1] 10405
# ---

# CHUNK 18
ego.instr <- induced.subgraph(karate,
   neighborhood(karate, 1, 1)[[1]])
ego.admin <- induced.subgraph(karate,
   neighborhood(karate, 1, 34)[[1]])
graph.density(karate)
# ---
## [1] 0.1390374
# ---
graph.density(ego.instr)
# ---
## [1] 0.25
# ---
graph.density(ego.admin)
# ---
## [1] 0.2091503
# ---

# CHUNK 19
transitivity(karate)
# ---
## [1] 0.2556818
# ---

# CHUNK 20
transitivity(karate, "local", vids=c(1,34))
# ---
## [1] 0.1500000 0.1102941
# ---

# CHUNK 21
reciprocity(aidsblog, mode="default")
# ---
## [1] 0.03278689
# ---
reciprocity(aidsblog, mode="ratio")
# ---
## [1] 0.01666667
# ---

# CHUNK 22
is.connected(yeast)
# ---
## [1] FALSE
# ---

# CHUNK 23
comps <- decompose.graph(yeast)
table(sapply(comps, vcount))
# ---
## 
##    2    3    4    5    6    7 2375 
##   63   13    5    6    1    3    1
# ---

# CHUNK 24
yeast.gc <- decompose.graph(yeast)[[1]]

# CHUNK 25
average.path.length(yeast.gc)
# ---
## [1] 5.09597
# ---

# CHUNK 26
diameter(yeast.gc)
# ---
## [1] 15
# ---

# CHUNK 27
transitivity(yeast.gc)
# ---
## [1] 0.4686663
# ---

# CHUNK 28
vertex.connectivity(yeast.gc)
# ---
## [1] 1
# ---
edge.connectivity(yeast.gc)
# ---
## [1] 1
# ---

# CHUNK 29
yeast.cut.vertices <- articulation.points(yeast.gc)
length(yeast.cut.vertices)
# ---
## [1] 350
# ---

# CHUNK 30
is.connected(aidsblog, mode=c("weak"))
# ---
## [1] TRUE
# ---

# CHUNK 31
is.connected(aidsblog, mode=c("strong"))
# ---
## [1] FALSE
# ---

# CHUNK 32
aidsblog.scc <- clusters(aidsblog, mode=c("strong"))
table(aidsblog.scc$csize)
# ---
## 
##   1   4 
## 142   1
# ---

# CHUNK 33
kc <- fastgreedy.community(karate)

# CHUNK 34
length(kc)
# ---
## [1] 3
# ---
sizes(kc)
# ---
## Community sizes
##  1  2  3 
## 18 11  5
# ---

# CHUNK 35
membership(kc)
# ---
##    Mr Hi  Actor 2  Actor 3  Actor 4  Actor 5  Actor 6 
##        2        2        2        2        3        3 
##  Actor 7  Actor 8  Actor 9 Actor 10 Actor 11 Actor 12 
##        3        2        1        1        3        2 
## Actor 13 Actor 14 Actor 15 Actor 16 Actor 17 Actor 18 
##        2        2        1        1        3        2 
## Actor 19 Actor 20 Actor 21 Actor 22 Actor 23 Actor 24 
##        1        2        1        2        1        1 
## Actor 25 Actor 26 Actor 27 Actor 28 Actor 29 Actor 30 
##        1        1        1        1        1        1 
## Actor 31 Actor 32 Actor 33   John A 
##        1        1        1        1
# ---

# CHUNK 36
plot(kc,karate)

# CHUNK 37
library(ape)
dendPlot(kc, mode="phylo")

# CHUNK 38
k.lap <- graph.laplacian(karate)
eig.anal <- eigen(k.lap)

# CHUNK 39
plot(eig.anal$values, col="blue",
   ylab="Eigenvalues of Graph Laplacian")

# CHUNK 40
f.vec <- eig.anal$vectors[, 33]

# CHUNK 41
faction <- get.vertex.attribute(karate, "Faction")
f.colors <- as.character(length(faction))
f.colors[faction == 1] <- "red"
f.colors[faction == 2] <- "cyan"
plot(f.vec, pch=16, xlab="Actor Number",
   ylab="Fiedler Vector Entry", col=f.colors)
abline(0, 0, lwd=2, col="lightgray")

# CHUNK 42
func.class <- get.vertex.attribute(yeast.gc, "Class")
table(func.class)
# ---
## func.class
##   A   B   C   D   E   F   G   M   O   P   R   T   U 
##  51  98 122 238  95 171  96 278 171 248  45 240 483
# ---

# CHUNK 43
yc <- fastgreedy.community(yeast.gc)
c.m <- membership(yc)

# CHUNK 44
table(c.m, func.class, useNA=c("no"))
# ---
##     func.class
## c.m    A   B   C   D   E   F   G   M   O   P   R   T   U
##   1    0   0   0   1   3   7   0   6   3 110   2  35  14
##   2    0   2   2   7   1   1   1   4  39   5   0   4  27
##   3    1   9   7  18   4   8   4  20  10  23   8  74  64
##   4   25  11  10  22  72  84  81 168  14  75  16  27 121
##   5    1   7   5  14   0   4   0   2   3   6   1  34  68
##   6    1  24   1   4   1   4   0   7   0   1   0  19  16
##   7    6  18   6  76   7   9   3   7   8   5   1   7  33
##   8    8  12  67  59   1  34   0  19  60  10   7   6  73
##   9    4   1   7   7   2  10   5   3   2   0   3   0  11
##   10   0   0   0   6   0   0   0   2   0   5   0  11   1
##   11   0   9   0  10   1   3   0   0   0   0   0   2   4
##   12   0   1   3   0   0   0   0   6  10   0   0   0   2
##   13   0   1   1   2   0   1   0   0   2   0   0  16  10
##   14   1   0   4   1   0   1   0   0   4   0   1   0  11
##   15   0   1   0   0   0   2   0   2   0   0   1   0   8
##   16   0   1   2   0   0   1   0   0  10   0   0   0   0
##   17   0   0   1   3   0   0   0   2   0   0   0   2   3
##   18   0   0   0   0   3   1   0   9   0   0   1   0   1
##   19   0   1   1   1   0   0   0   0   0   0   0   0   3
##   20   0   0   0   6   0   0   0   1   0   0   0   1   2
##   21   1   0   0   0   0   0   0   0   6   0   0   1   0
##   22   0   0   0   0   0   0   0   1   0   0   0   0   8
##   23   0   0   0   0   0   0   0   4   0   0   0   0   0
##   24   0   0   0   0   0   0   2   2   0   0   0   1   0
##   25   0   0   0   0   0   0   0   5   0   0   0   0   0
##   26   0   0   1   0   0   0   0   4   0   0   1   0   1
##   27   3   0   4   0   0   1   0   0   0   0   0   0   0
##   28   0   0   0   0   0   0   0   0   0   6   0   0   0
##   29   0   0   0   1   0   0   0   1   0   0   3   0   0
##   30   0   0   0   0   0   0   0   0   0   2   0   0   2
##   31   0   0   0   0   0   0   0   3   0   0   0   0   0
# ---

# CHUNK 45
assortativity.nominal(yeast, (V(yeast)$Class=="P")+1, 
   directed=FALSE)
# ---
## [1] 0.4965229
# ---

# CHUNK 46
assortativity.degree(yeast)
# ---
## [1] 0.4610798
# ---

