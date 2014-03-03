# SAND with R, chapter3.tex

# CHUNK 1
library(sand)
g.l <- graph.lattice(c(5, 5, 5))

# CHUNK 2
data(aidsblog)
summary(aidsblog)
# ---
## IGRAPH D--- 146 187 --
# ---

# CHUNK 3
igraph.options(vertex.size=3, vertex.label=NA,
   edge.arrow.size=0.5)
par(mfrow=c(1, 2))
plot(g.l, layout=layout.circle)
title("5x5x5 Lattice")
plot(aidsblog, layout=layout.circle)
title("Blog Network")

# CHUNK 4
plot(g.l,layout=layout.fruchterman.reingold)
title("5x5x5 Lattice")
plot(aidsblog,layout=layout.fruchterman.reingold)
title("Blog Network")

# CHUNK 5
plot(g.l, layout=layout.kamada.kawai)
title("5x5x5 Lattice")
plot(aidsblog, layout=layout.kamada.kawai)
title("Blog Network")

# CHUNK 6
g.tree <- graph.formula(1-+2,1-+3,1-+4,2-+5,2-+6,2-+7,
                         3-+8,3-+9,4-+10)
par(mfrow=c(1, 3))
igraph.options(vertex.size=30, edge.arrow.size=0.5,
   vertex.label=NULL)
plot(g.tree, layout=layout.circle)
plot(g.tree, layout=layout.reingold.tilford(g.tree, 
   circular=T))
plot(g.tree, layout=layout.reingold.tilford)

# CHUNK 7
plot(g.bip, layout=-layout.bipartite(g.bip)[,2:1], 
   vertex.size=30, vertex.shape=ifelse(V(g.bip)$type, 
     "rectangle", "circle"),
   vertex.color=ifelse(V(g.bip)$type, "red", "cyan"))

# CHUNK 8
library(igraphdata)
data(karate)
# Reproducible layout
set.seed(42)
l <- layout.kamada.kawai(karate)
# Plot undecorated first.
par(mfrow=c(1,1))
plot(karate, layout=l, vertex.label=NA)
# Now decorate, starting with labels.
V(karate)$label <- sub("Actor ", "", V(karate)$name)
# Two leaders get shapes different from club members.
V(karate)$shape <- "circle"
V(karate)[c("Mr Hi", "John A")]$shape <- "rectangle"
# Differentiate two factions by color.
V(karate)[Faction == 1]$color <- "red"
V(karate)[Faction == 2]$color <- "dodgerblue"
# Vertex area proportional to vertex strength
# (i.e., total weight of incident edges).
V(karate)$size <- 4*sqrt(graph.strength(karate))
V(karate)$size2 <- V(karate)$size * .5
# Weight edges by number of common activities
E(karate)$width <- E(karate)$weight
# Color edges by within/between faction.
F1 <- V(karate)[Faction==1]
F2 <- V(karate)[Faction==2]
E(karate)[ F1 %--% F1 ]$color <- "pink"
E(karate)[ F2 %--% F2 ]$color <- "lightblue"
E(karate)[ F1 %--% F2 ]$color <- "yellow"
# Offset vertex labels for smaller points (default=0).
V(karate)$label.dist <- 
    ifelse(V(karate)$size >= 10, 0, 0.75)
# Plot decorated graph, using same layout.
plot(karate, layout=l)

# CHUNK 9
library(sand)
data(lazega)
# Office location indicated by color.
colbar <- c("red", "dodgerblue", "goldenrod")
v.colors <- colbar[V(lazega)$Office]
# Type of practice indicated by vertex shape.
v.shapes <- c("circle", "square")[V(lazega)$Practice]
# Vertex size proportional to years with firm.
v.size <- 3.5*sqrt(V(lazega)$Years)
set.seed(42)
l <- layout.fruchterman.reingold(lazega)
plot(lazega, layout=l, vertex.color=v.colors,
   vertex.shape=v.shapes, vertex.size=v.size)

# CHUNK 10
library(sand)
summary(fblog)
# ---
## IGRAPH UN-- 192 1431 -- 
## attr: name (v/c), PolParty (v/c)
# ---

# CHUNK 11
party.names <- sort(unique(V(fblog)$PolParty))
party.names
# ---
## [1] " Cap21"                   " Commentateurs Analystes"
## [3] " Les Verts"               " liberaux"               
## [5] " Parti Radical de Gauche" " PCF - LCR"              
## [7] " PS"                      " UDF"                    
## [9] " UMP"
# ---

# CHUNK 12
set.seed(42)
l = layout.kamada.kawai(fblog)
party.nums.f <- as.factor(V(fblog)$PolParty)
party.nums <- as.numeric(party.nums.f)
plot(fblog, layout=l, vertex.label=NA,
   vertex.color=party.nums, vertex.size=3)

# CHUNK 13
set.seed(42)
l <- layout.drl(fblog)
plot(fblog, layout=l, vertex.size=5, vertex.label=NA,
   vertex.color=party.nums)

# CHUNK 14
fblog.c <- contract.vertices(fblog, party.nums)
E(fblog.c)$weight <- 1
fblog.c <- simplify(fblog.c)

# CHUNK 15
party.size <- as.vector(table(V(fblog)$PolParty))
plot(fblog.c, vertex.size=5*sqrt(party.size),
   vertex.label=party.names,
   vertex.color=V(fblog.c),
   edge.width=sqrt(E(fblog.c)$weight),
   vertex.label.dist=1.5, edge.arrow.size=0)

# CHUNK 16
k.nbhds <- graph.neighborhood(karate, order=1)

# CHUNK 17
sapply(k.nbhds, vcount)
# ---
##  [1] 17 10 11  7  4  5  5  5  6  3  4  2  3  6  3  3  3
## [18]  3  3  4  3  3  3  6  4  4  3  5  4  5  5  7 13 18
# ---

# CHUNK 18
k.1 <- k.nbhds[[1]]
k.34 <- k.nbhds[[34]]
par(mfrow=c(1,2))
plot(k.1, vertex.label=NA,
   vertex.color=c("red", rep("lightblue", 16)))
plot(k.34, vertex.label=NA,
   vertex.color=c(rep("lightblue", 17), "red"))

