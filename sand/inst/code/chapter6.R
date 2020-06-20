# SAND with R, chapter6.tex

# CHUNK 1
library(sand)
data(lazega)
A <- as_adjacency_matrix(lazega)
v.attrs <- as_data_frame(lazega, what="vertices")

# CHUNK 2
library(ergm)  # Will load package 'network' as well.
lazega.s <- network::as.network(as.matrix(A),
  directed=FALSE)
network::set.vertex.attribute(lazega.s, "Office",
   v.attrs$Office)
network::set.vertex.attribute(lazega.s, "Practice",
   v.attrs$Practice)
network::set.vertex.attribute(lazega.s, "Gender",
   v.attrs$Gender)
network::set.vertex.attribute(lazega.s, "Seniority",
   v.attrs$Seniority)

# CHUNK 3
my.ergm.bern <- formula(lazega.s ~ edges)
my.ergm.bern
# ---
## lazega.s ~ edges
# ---

# CHUNK 4
summary(my.ergm.bern)
# ---
## edges
##   115
# ---

# CHUNK 5
my.ergm <- formula(lazega.s ~ edges + kstar(2)
   + kstar(3) + triangle)
summary(my.ergm)
# ---
##    edges   kstar2   kstar3 triangle
##      115      926     2681      120
# ---
     
# CHUNK 6
my.ergm <- formula(lazega.s ~ edges
   + gwesp(1, fixed=TRUE))
summary(my.ergm)
# ---
##         edges gwesp.fixed.1
##      115.0000      213.1753
# ---

# CHUNK 7
lazega.ergm <- formula(lazega.s ~ edges
   + gwesp(log(3), fixed=TRUE)
   + nodemain("Seniority")
   + nodemain("Practice")
   + match("Practice")
   + match("Gender")
   + match("Office"))

# CHUNK 8
set.seed(42)
lazega.ergm.fit <- ergm(lazega.ergm)

# CHUNK 9
anova(lazega.ergm.fit)
# ---
## Analysis of Variance Table
##
## Model 1: lazega.s ~ edges + gwesp(log(3), fixed = TRUE) + 
##     nodemain("Seniority") + nodemain("Practice") + 
##     match("Practice") + match("Gender") + 
##     match("Office")
##          Df Deviance Resid. Df Resid. Dev Pr(>|Chisq|)    
## NULL                       630     873.37                 
## Model 1:  7   413.74       623     459.63    < 2.2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ---

# CHUNK 10
summary(lazega.ergm.fit)
# ---
## ==========================
## Summary of model fit
## ==========================
##
## Formula:   lazega.s ~ edges + gwesp(log(3), fixed = TRUE) + 
##     nodemain("Seniority") + nodemain("Practice") + 
##     match("Practice") + match("Gender") + 
##     match("Office")
##
## Iterations:  2 out of 20 
## 
## Monte Carlo MLE Results:
##                              Estimate Std. Error MCMC %      
## edges                        -7.00655    0.67114      0   
## gwesp.fixed.1.09861228866811  0.59166    0.08554      0     
## nodecov.Seniority             0.02456    0.00620      0     
## nodecov.Practice              0.39455    0.10218      0    
## nodematch.Practice            0.76966    0.19060      0     
## nodematch.Gender              0.73767    0.24362      0     
## nodematch.Office              1.16439    0.18753      0     
##
##                               z value     Pr(>|z|)
## edges                         -10.440    < 1e-04 ***
## gwesp.fixed.1.09861228866811    6.917    < 1e-04 ***
## nodecov.Seniority               3.962    < 1e-04 ***
## nodecov.Practice                3.861    0.000113 ***
## nodematch.Practice              4.038    < 1e-04 ***
## nodematch.Gender                3.028    0.002463 **
## nodematch.Office                6.209    < 1e-04 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
##      Null Deviance: 873.4  on 630  degrees of freedom
##  Residual Deviance: 459.6  on 623  degrees of freedom
## 
## AIC: 473.6    BIC: 504.7    (Smaller is better.)
# ---

# CHUNK 11
gof.lazega.ergm <- gof(lazega.ergm.fit)

# CHUNK 12
plot(gof.lazega.ergm)

# CHUNK 13
library(blockmodels)
set.seed(42)
A.fblog <- as.matrix(as_adjacency_matrix(fblog))
fblog.sbm <- BM_bernoulli("SBM_sym", A.fblog, 
                           verbosity=0, plotting='')
fblog.sbm$estimate()

# CHUNK 14
ICLs <- fblog.sbm$ICL
Q <- which.max(ICLs)
Q
# ---
## [1] 10
# ---

# CHUNK 15
Z <- fblog.sbm$memberships[[Q]]$Z

# CHUNK 16
cl.labs <- apply(Z,1,which.max)

# CHUNK 17
nv <- vcount(fblog)
summary(Z[cbind(1:nv,cl.labs)])
# ---
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.8586  0.9953  0.9953  0.9938  0.9953  0.9953
# ---

# CHUNK 18
cl.cnts <- as.vector(table(cl.labs))
alpha <- cl.cnts/nv
alpha
# ---
##  [1] 0.18229167 0.14062500 0.05729167 0.10937500 
##  [5] 0.12500000 0.13020833 0.03125000 0.03645833 
##  [9] 0.06770833 0.11979167
# ---

# CHUNK 19
Pi.mat <- fblog.sbm$model_parameters[[Q]]$pi
Pi.mat[3,]
# ---
##  [1] 0.0030340287 0.0073657690 0.9102251927 0.0009221811 
##  [5] 0.0009170384 0.0364593875 0.0177621832 0.0024976022 
##  [9] 0.0431732528 0.0012495852
# ---

# CHUNK 20
ntrials <- 1000
Pi.mat <- (t(Pi.mat)+Pi.mat)/2
deg.summ <- list(ntrials)
for(i in (1:ntrials)){
   blk.sz <- rmultinom(1,nv,alpha)
   g.sbm <- sample_sbm(nv,pref.matrix=Pi.mat,
                       block.sizes=blk.sz,
                       directed=FALSE)
   deg.summ[[i]] <- summary(degree(g.sbm))
 }
Reduce('+',deg.summ)/ntrials
# ---
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.931   9.165  13.127  15.183  18.896  49.484 
# ---
summary(degree(fblog))
# ---
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    2.00    8.00   13.00   14.91   18.00   56.00
# ---

# CHUNK 21
plot(fblog.sbm$ICL,xlab="Q",ylab="ICL",type="b")
lines(c(Q,Q),c(min(ICLs),max(ICLs)),col="red",lty=2)

# CHUNK 22
edges <- as_edgelist(fblog,names=FALSE)
neworder<-order(cl.labs)
m<-t(matrix(order(neworder)[as.numeric(edges)],2))
plot(1, 1, xlim = c(0, nv + 1), ylim = c(nv + 1, 0), 
      type = "n", axes= FALSE, xlab="Classes",
      ylab="Classes",
      main="Reorganized Adjacency matrix")
rect(m[,2]-0.5,m[,1]-0.5,m[,2]+0.5,m[,1]+0.5,col=1)
rect(m[,1]-0.5,m[,2]-0.5,m[,1]+0.5,m[,2]+0.5,col=1)
cl.lim <- cl.cnts 
cl.lim <- cumsum(cl.lim)[1:(length(cl.lim)-1)]+0.5
clip(0,nv+1,nv+1,0)
abline(v=c(0.5,cl.lim,nv+0.5),
        h=c(0.5,cl.lim,nv+0.5),col="red")

# CHUNK 23
g.cl <- graph_from_adjacency_matrix(Pi.mat,
                                     mode="undirected",
                                     weighted=TRUE)
# Set necessary parameters
vsize <- 100*sqrt(alpha)
ewidth <- 10*E(g.cl)$weight
PolP <- V(fblog)$PolParty
class.by.PolP <- as.matrix(table(cl.labs,PolP))
pie.vals <- lapply(1:Q, function(i) 
                    as.vector(class.by.PolP[i,]))
my.cols <- topo.colors(length(unique(PolP)))
# Plot 
plot(g.cl, edge.width=ewidth, 
      vertex.shape="pie", vertex.pie=pie.vals, 
      vertex.pie.color=list(my.cols),
      vertex.size=vsize, vertex.label.dist=0.1*vsize,
      vertex.label.degree=pi)
# Add a legend
my.names <- names(table(PolP))
my.names[2] <- "Comm. Anal."
my.names[5] <- "PR de G"
legend(x="topleft", my.names,
        fill=my.cols, bty="n")

# CHUNK 24
summary(lazega)
# ---
## IGRAPH NA UN-- 36 115 -- 
## + attr: name (v/c), Seniority (v/n), Status (v/n), 
##     Gender (v/n), Office (v/n), Years (v/n), Age (v/n), 
##     Practice (v/n), School (v/n)
# ---

# CHUNK 25
library(eigenmodel)
set.seed(42)
A <- as_adjacency_matrix(lazega, sparse=FALSE)
lazega.leig.fit1 <- eigenmodel_mcmc(A, R=2, S=11000,
   burn=10000)

# CHUNK 26
same.prac.op <- v.attr.lazega$Practice %o%
   v.attr.lazega$Practice
same.prac <- matrix(as.numeric(same.prac.op
   %in% c(1, 4, 9)), 36, 36)
same.prac <- array(same.prac,dim=c(36, 36, 1))

# CHUNK 27
lazega.leig.fit2 <- eigenmodel_mcmc(A, same.prac, R=2,
   S=11000,burn=10000)

# CHUNK 28
same.off.op <- v.attr.lazega$Office %o%
   v.attr.lazega$Office
same.off <- matrix(as.numeric(same.off.op %in%
   c(1, 4, 9)), 36, 36)
same.off <- array(same.off,dim=c(36, 36, 1))
lazega.leig.fit3 <- eigenmodel_mcmc(A, same.off,
    R=2, S=11000, burn=10000)

# CHUNK 29
lat.sp.1 <-
   eigen(lazega.leig.fit1$ULU_postmean)$vec[, 1:2]
lat.sp.2 <-
   eigen(lazega.leig.fit2$ULU_postmean)$vec[, 1:2]
lat.sp.3 <-
   eigen(lazega.leig.fit3$ULU_postmean)$vec[, 1:2]

# CHUNK 30
colbar <- c("red", "dodgerblue", "goldenrod")
v.colors <- colbar[V(lazega)$Office]
v.shapes <- c("circle", "square")[V(lazega)$Practice]
v.size <- 3.5*sqrt(V(lazega)$Years)
v.label <- V(lazega)$Seniority
plot(lazega, layout=lat.sp.1, vertex.color=v.colors,
      vertex.shape=v.shapes, vertex.size=v.size,
      vertex.label=(1:36))

# CHUNK 31
perm.index <- sample(1:630)
nfolds <- 5
nmiss <- 630/nfolds
Avec <- A[lower.tri(A)]
Avec.pred1 <- numeric(length(Avec))

# CHUNK 32
for(i in seq(1,nfolds)){
  # Index of missing values.
  miss.index <- seq(((i-1) * nmiss + 1),
     (i*nmiss), 1)
  A.miss.index <- perm.index[miss.index]

  # Fill a new Atemp appropriately with NA's.
  Avec.temp <- Avec
  Avec.temp[A.miss.index] <-
     rep("NA", length(A.miss.index))
  Avec.temp <- as.numeric(Avec.temp)
  Atemp <- matrix(0, 36, 36)
  Atemp[lower.tri(Atemp)] <- Avec.temp
  Atemp <- Atemp + t(Atemp)

  # Now fit model and predict.
  Y <- Atemp

  model1.fit <- eigenmodel_mcmc(Y, R=2,
     S=11000, burn=10000)
  model1.pred <- model1.fit$Y_postmean
  model1.pred.vec <-
     model1.pred[lower.tri(model1.pred)]
  Avec.pred1[A.miss.index] <-
     model1.pred.vec[A.miss.index]
}

# CHUNK 33
library(ROCR)
pred1 <- prediction(Avec.pred1, Avec)
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1, col="blue", lwd=3)

# CHUNK 34
perf1.auc <- performance(pred1, "auc")
slot(perf1.auc, "y.values")
# ---
## [[1]]
## [1] 0.8191811
# ---

