# SAND with R, chapter8.tex

# CHUNK 1
set.seed(42)
library(sand)
data(ppi.CC)

# CHUNK 2
summary(ppi.CC)
# ---
## IGRAPH UN-- 134 241 -- 
## attr: name (v/c), ICSC (v/n), IPR000198 (v/n),
##   IPR000403 (v/n), IPR001806 (v/n), IPR001849
##   (v/n), IPR002041 (v/n), IPR003527 (v/n)
# ---

# CHUNK 3
V(ppi.CC)$ICSC[1:10]
# ---
##  [1] 1 1 1 1 1 0 1 1 1 1
# ---

# CHUNK 4
V(ppi.CC)[ICSC == 1]$color <- "yellow"
V(ppi.CC)[ICSC == 0]$color <- "blue"
plot(ppi.CC, vertex.size=5, vertex.label=NA)

# CHUNK 5
clu <- clusters(ppi.CC)
ppi.CC.gc <- induced.subgraph(ppi.CC, 
   clu$membership==which.max(clu$csize))
nn.ave <- sapply(V(ppi.CC.gc), 
   function(x) mean(V(ppi.CC.gc)[nei(x)]$ICSC))

# CHUNK 6
par(mfrow=c(2,1))
hist(nn.ave[V(ppi.CC.gc)$ICSC == 1], col="yellow", 
   ylim=c(0, 30), xlab="Proportion Neighbors w/ ICSC", 
   main="Egos w/ ICSC")
hist(nn.ave[V(ppi.CC.gc)$ICSC == 0], col="blue", 
   ylim=c(0, 30), xlab="Proportion Neighbors w/ ICSC", 
   main="Egos w/out ICSC")

# CHUNK 7
nn.pred <- as.numeric(nn.ave > 0.5)
mean(as.numeric(nn.pred != V(ppi.CC.gc)$ICSC))
# ---
## [1] 0.2598425
# ---

# CHUNK 8
source("http://bioconductor.org/biocLite.R")
biocLite("GOstats", suppressAutoUpdate=TRUE,
   suppressUpdates=TRUE)
library(GOstats)
library(GO.db)

# CHUNK 9
biocLite("org.Sc.sgd.db", suppressAutoUpdate=TRUE, 
   suppressUpdates=TRUE)
library(org.Sc.sgd.db)

# CHUNK 10
x <- as.list(org.Sc.sgdGO2ALLORFS)
current.icst <- x[names(x) == "GO:0035556"]
ev.code <- names(current.icst[[1]])
icst.ida <- current.icst[[1]][ev.code == "IDA"]

# CHUNK 11
orig.icsc <- V(ppi.CC.gc)[ICSC == 1]$name

# CHUNK 12
candidates <- intersect(icst.ida, V(ppi.CC.gc)$name)

# CHUNK 13
new.icsc <- setdiff(candidates, orig.icsc)
new.icsc
# ---
## [1] "YDL159W" "YHL007C" "YIL033C" "YLR362W"
# ---

# CHUNK 14
nn.ave[V(ppi.CC.gc)$name %in% new.icsc]
# ---
## [1] 0.7500000 0.4166667 0.3333333 0.8750000
# ---

# CHUNK 15
library(ngspatial)

# CHUNK 16
X <- V(ppi.CC.gc)$ICSC
A <- get.adjacency(ppi.CC.gc, sparse=FALSE)

# CHUNK 17
formula1 <- X~1

# CHUNK 18
gene.motifs <- cbind(V(ppi.CC.gc)$IPR000198,
                      V(ppi.CC.gc)$IPR000403,
                      V(ppi.CC.gc)$IPR001806,
                      V(ppi.CC.gc)$IPR001849,
                      V(ppi.CC.gc)$IPR002041,
                      V(ppi.CC.gc)$IPR003527)
formula2 <- X ~ gene.motifs

# CHUNK 19
m1.mrf <- autologistic(formula1, A=A, 
   control=list(confint="none"))

# CHUNK 20
m1.mrf$coefficients
# ---
## (Intercept)         eta 
##   0.2004949   1.1351942
# ---

# CHUNK 21
mrf1.pred <- as.numeric((m1.mrf$fitted.values > 0.5))

# CHUNK 22
mean(as.numeric(mrf1.pred != V(ppi.CC.gc)$ICSC))
# ---
## [1] 0.2047244
# ---

# CHUNK 23
m1.mrf$fitted.values[V(ppi.CC.gc)$name %in% new.icsc]
# ---
## [1] 0.7519142 0.1658647 0.2184092 0.9590030
# ---

# CHUNK 24
m2.mrf <- autologistic(formula2, A=A, 
   control=list(confint="none"))

# CHUNK 25
m2.mrf$coefficients
# ---
##   (Intercept)  gene.motifs1  gene.motifs2  gene.motifs3 
##  5.081573e-02  1.876848e+00  1.875217e+01  1.875217e+01 
##  gene.motifs4  gene.motifs5  gene.motifs6           eta 
##  1.824990e+01  8.492393e-08 -1.837997e+01  1.297921e+00
# ---

# CHUNK 26
mrf.pred2 <- as.numeric((m2.mrf$fitted.values > 0.5))
mean(as.numeric(mrf.pred2 != V(ppi.CC.gc)$ICSC))
# ---
## [1] 0.1889764
# ---

# CHUNK 27
m2.mrf$fitted.values[V(ppi.CC.gc)$name %in% new.icsc]
# ---
## [1] 0.7829254 0.4715219 0.4962188 0.7829254
# ---

# CHUNK 28
srand(42)       # random seed for rautologistic
ntrials <- 100
a1.mrf <- numeric(ntrials)
a2.mrf <- numeric(ntrials)
Z1 <- rep(1,length(X))
Z2 <- cbind(Z1, gene.motifs)
for(i in 1:ntrials){
   X1.mrf <- rautologistic(as.matrix(Z1), A=A,
                           theta=m1.mrf$coefficients)
   X2.mrf<- rautologistic(as.matrix(Z2), A=A,
                           theta=m2.mrf$coefficients)
   a1.mrf[i] <- assortativity(ppi.CC.gc, X1.mrf+1,
                               directed=FALSE)
   a2.mrf[i] <- assortativity(ppi.CC.gc, X2.mrf+1,
                               directed=FALSE)
 }

# CHUNK 29
assortativity(ppi.CC.gc, X+1, directed=FALSE)
# ---
## [1] 0.3739348
# ---

# CHUNK 30
summary(a1.mrf)
# ---
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.09915 0.22210 0.28050 0.28900 0.35300 0.47980
# ---
summary(a2.mrf)
# ---
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.04049 0.20570 0.25750 0.26020 0.32080 0.48150
# ---

# CHUNK 31
L <- as.matrix(graph.laplacian(ppi.CC.gc))
e.L <- eigen(L)
nv <- vcount(ppi.CC.gc)
e.vals <- e.L$values[1:(nv-1)]
f.e.vals <- c((e.vals)^(-1), 0)
plot(f.e.vals, col="magenta", xlim=c(1, nv),
   xlab=c("Index i"), ylab=expression(f(gamma[i])))

# CHUNK 32
e.vec <- e.L$vectors[, (nv-1)]
v.colors <- character(nv)
v.colors[e.vec >= 0] <- "red"
v.colors[e.vec < 0] <- "blue"
v.size <- 15 * sqrt(abs(e.vec))
l <- layout.fruchterman.reingold(ppi.CC.gc)
plot(ppi.CC.gc, layout=l, vertex.color=v.colors,
   vertex.size=v.size, vertex.label=NA)

# CHUNK 33
library(kernlab)

# CHUNK 34
K1.tmp <- e.L$vectors %*% diag(f.e.vals) %*%
   t(e.L$vectors)
K1 <- as.kernelMatrix(K1.tmp)

# CHUNK 35
K.motifs <- gene.motifs %*% t(gene.motifs)

# CHUNK 36
K2.tmp <- 0.5 * K1.tmp + 0.5 * K.motifs
K2 <- as.kernelMatrix(K2.tmp)

# CHUNK 37
m1.svm <- ksvm(K1, X, type="C-svc")
m1.svm.fitted <- fitted(m1.svm)

# CHUNK 38
mean(as.numeric(m1.svm.fitted != V(ppi.CC.gc)$ICSC))
# ---
## [1] 0.1102362
# ---

# CHUNK 39
m1.svm.fitted[V(ppi.CC.gc)$name %in% new.icsc]
# ---
## [1] 1 1 1 1
# ---

# CHUNK 40
m2.svm <- ksvm(K2, X, type="C-svc")

# CHUNK 41
m2.svm.fitted <- fitted(m2.svm)
mean(as.numeric(m2.svm.fitted != V(ppi.CC.gc)$ICSC))
# ---
## [1] 0.06299213
# ---

# CHUNK 42
m2.svm.fitted[V(ppi.CC.gc)$name %in% new.icsc]
# ---
## [1] 1 0 0 1
# ---

# CHUNK 43
gl <- list()
gl$ba <- barabasi.game(250, m=5, directed=FALSE)
gl$er <- erdos.renyi.game(250, 1250, type=c("gnm"))
gl$ws <- watts.strogatz.game(1, 100, 12, 0.01)

# CHUNK 44
beta <- 0.5
gamma <- 1

# CHUNK 45
ntrials <- 100

# CHUNK 46
sim <- lapply(gl, sir, beta=beta, gamma=gamma, 
   no.sim=ntrials)

# CHUNK 47
plot(sim$er)
plot(sim$ba, color="palegoldenrod", 
   median_color="gold", quantile_color="gold")
plot(sim$ws, color="pink", median_color="red", 
   quantile_color="red")

# CHUNK 48
x.max <- max(sapply(sapply(sim, time_bins), max))
y.max <- 1.05 * max(sapply(sapply(sim, function(x)
   median(x)[["NI"]]), max, na.rm=TRUE))
plot(time_bins(sim$er), median(sim$er)[["NI"]], 
   type="l", lwd=2, col="blue", xlim=c(0, x.max), 
   ylim=c(0, y.max), xlab="Time", 
   ylab=expression(N[I](t)))
lines(time_bins(sim$ba), median(sim$ba)[["NI"]],
   lwd=2, col="gold")
lines(time_bins(sim$ws), median(sim$ws)[["NI"]],
   lwd=2, col="red")
legend("topright", c("ER", "BA", "WS"), 
   col=c("blue", "gold", "red"), lty=1)

