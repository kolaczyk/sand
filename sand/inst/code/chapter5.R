# SAND with R, chapter5.tex

# CHUNK 1
library(sand)
set.seed(42)
g.er <- erdos.renyi.game(100, 0.02)
plot(g.er, layout=layout.circle, vertex.label=NA)

# CHUNK 2
is.connected(g.er)
# ---
## [1] FALSE
# ---

# CHUNK 3
table(sapply(decompose.graph(g.er), vcount))
# ---
## 
##  1  2  3  4 71 
## 15  2  2  1  1
# ---

# CHUNK 4
mean(degree(g.er))
# ---
## [1] 1.9
# ---

# CHUNK 5
hist(degree(g.er), col="lightblue",
   xlab="Degree", ylab="Frequency", main="")

# CHUNK 6
average.path.length(g.er)
# ---
## [1] 5.276511
# ---
diameter(g.er)
# ---
## [1] 14
# ---

# CHUNK 7
transitivity(g.er)
# ---
## [1] 0.01639344
# ---

# CHUNK 8
degs <- c(2,2,2,2,3,3,3,3)
g1 <- degree.sequence.game(degs, method="vl")
g2 <- degree.sequence.game(degs, method="vl")
plot(g1, vertex.label=NA)
plot(g2, vertex.label=NA)

# CHUNK 9
graph.isomorphic(g1, g2)
# ---
## [1] FALSE
# ---

# CHUNK 10
c(ecount(g1), ecount(g2))
# ---
## [1] 10 10
# ---

# CHUNK 11
data(yeast)
degs <- degree(yeast)
fake.yeast <- degree.sequence.game(degs, 
   method=c("vl"))
all(degree(yeast) == degree(fake.yeast))
# ---
## [1] TRUE
# ---

# CHUNK 12
diameter(yeast)
# ---
## [1] 15
# ---
diameter(fake.yeast)
# ---
## [1] 7
# ---

# CHUNK 13
transitivity(yeast)
# ---
## [1] 0.4686178
# ---
transitivity(fake.yeast)
# ---
## [1] 0.03968903
# ---

# CHUNK 14
g.ws <- watts.strogatz.game(1, 25, 5, 0.05)
plot(g.ws, layout=layout.circle, vertex.label=NA)

# CHUNK 15
g.lat100 <- watts.strogatz.game(1, 100, 5, 0)
transitivity(g.lat100)
# ---
## [1] 0.6666667
# ---

# CHUNK 16
diameter(g.lat100)
# ---
## [1] 10
# ---
average.path.length(g.lat100)
# ---
## [1] 5.454545
# ---

# CHUNK 17
g.ws100 <- watts.strogatz.game(1, 100, 5, 0.05)
diameter(g.ws100)
# ---
## [1] 4
# ---
average.path.length(g.ws100)
# ---
## [1] 2.669091
# ---
transitivity(g.ws100)
# ---
## [1] 0.4864154
# ---

# CHUNK 18
steps <- seq(-4, -0.5, 0.1)
len <- length(steps)
cl <- numeric(len)
apl <- numeric(len)
ntrials <- 100
function(x) {
for (i in (1:len)) {
   cltemp <- numeric(ntrials)
   apltemp <- numeric(ntrials)
   for (j in (1:ntrials)) {
     g <- watts.strogatz.game(1, 1000, 10, 10^steps[i])
     cltemp[j] <- transitivity(g)
     apltemp[j] <- average.path.length(g)
   }
   cl[i] <- mean(cltemp)
   apl[i] <- mean(apltemp)
 }
}
cl <- c(0.710063379997766, 0.709978692214238, 0.709907143256545, 0.709724130686251, 
0.709438119171845, 0.709084388626035, 0.708846266062516, 0.70839051192321, 
0.707759691875033, 0.707113107172047, 0.706190905933217, 0.705111695935303, 
0.703784841816035, 0.702347962546443, 0.699998029666335, 0.696966876979092, 
0.693486200400489, 0.689434391992611, 0.683909800354255, 0.676998368887877, 
0.669280399418907, 0.657931797843006, 0.645296561052957, 0.628819148376097, 
0.609573848258676, 0.585356133848734, 0.55633728515175, 0.521088308467222, 
0.480754321558662, 0.433680652553029, 0.378558209487185, 0.318761294080951, 
0.25616767320489, 0.193136725458156, 0.134572797222469, 0.0849655822333312)
apl <- c(19.2309712512513, 18.1696153953954, 17.7627795195195, 16.6679303103103, 
14.5979843643644, 12.8854347747748, 12.1038251251251, 11.2341607007007, 
9.82806862862863, 9.11716512512512, 8.24078900900901, 7.61965115115115, 
7.0006803003003, 6.52964078078078, 6.03792086086086, 5.60314024024024, 
5.26338822822823, 4.98922616616617, 4.70584088088088, 4.45406394394394, 
4.26384696696697, 4.05971747747748, 3.89097137137137, 3.72829705705706, 
3.58416728728729, 3.45041687687688, 3.33307095095095, 3.22760666666667, 
3.12912454454454, 3.0362582982983, 2.94736488488488, 2.87122124124124, 
2.80854338338338, 2.75799567567568, 2.71760948948949, 2.68516954954955)

# CHUNK 19
plot(steps, cl/max(cl), ylim=c(0, 1), lwd=3, type="l", 
   col="blue", xlab=expression(log[10](p)),
   ylab="Clustering and Average Path Length")
lines(steps, apl/max(apl), lwd=3, col="red")

# CHUNK 20
set.seed(42)
g.ba <- barabasi.game(100, directed=FALSE)

# CHUNK 21
plot(g.ba, layout=layout.circle, vertex.label=NA)

# CHUNK 22
hist(degree(g.ba), col="lightblue",
   xlab="Degree", ylab="Frequency", main="")

# CHUNK 23
summary(degree(g.ba))
# ---
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    1.00    1.00    1.98    2.00   11.00
# ---

# CHUNK 24
average.path.length(g.ba)
# ---
## [1] 4.923434
# ---
diameter(g.ba)
# ---
## [1] 9
# ---

# CHUNK 25
transitivity(g.ba)
# ---
## [1] 0
# ---

# CHUNK 26
data(karate)
nv <- vcount(karate)
ne <- ecount(karate)
degs <- degree(karate)

# CHUNK 27
ntrials <- 1000

# CHUNK 28
num.comm.rg <- numeric(ntrials)
for(i in (1:ntrials)){
   g.rg <- erdos.renyi.game(nv, ne, type="gnm")
   c.rg <- fastgreedy.community(g.rg)
   num.comm.rg[i] <- length(c.rg)
 }

# CHUNK 29
num.comm.grg <- numeric(ntrials)
for(i in (1:ntrials)){
   g.grg <- degree.sequence.game(degs, method="vl")
   c.grg <- fastgreedy.community(g.grg)
   num.comm.grg[i] <- length(c.grg)
 }

# CHUNK 30
rslts <- c(num.comm.rg,num.comm.grg)
indx <- c(rep(0, ntrials), rep(1, ntrials))
counts <- table(indx, rslts)/ntrials
barplot(counts, beside=TRUE, col=c("blue", "red"),
   xlab="Number of Communities", 
   ylab="Relative Frequency",
   legend=c("Fixed Size", "Fixed Degree Sequence"))

# CHUNK 31
library(igraphdata)
data(macaque)
summary(macaque)
# ---
## IGRAPH DN-- 45 463 -- 
## attr: Citation (g/c), Author (g/c), shape (v/c),
##   name (v/c)
# ---

# CHUNK 32
clust.coef.dir <- function(graph) {
   A <- as.matrix(get.adjacency(graph))
   S <- A + t(A)
   deg <- degree(graph, mode=c("total"))
   num <- diag(S %*% S %*% S)
   denom <- diag(A %*% A)
   denom <- 2 * (deg * (deg - 1) - 2 * denom)
   cl <- mean(num/denom)
   return(cl)
 }

# CHUNK 33
ntrials <- 1000
nv <- vcount(macaque)
ne <- ecount(macaque)
cl.rg <- numeric(ntrials)
apl.rg <- numeric(ntrials)
for (i in (1:ntrials)) {
   g.rg <- erdos.renyi.game(nv, ne, type="gnm",
      directed=TRUE)
   cl.rg[i] <- clust.coef.dir(g.rg)
   apl.rg[i] <- average.path.length(g.rg)
 }

# CHUNK 34
summary(cl.rg)
# ---
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.2159  0.2302  0.2340  0.2340  0.2377  0.2548
# ---

# CHUNK 35
summary(apl.rg)
# ---
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.810   1.827   1.833   1.833   1.838   1.858
# ---

# CHUNK 36
clust.coef.dir(macaque)
# ---
## [1] 0.5501073
# ---
average.path.length(macaque)
# ---
## [1] 2.148485
# ---

