# SAND with R, chapter6.tex

# CHUNK 1
library(sand)
data(lazega)
A <- get.adjacency(lazega)
v.attrs <- get.data.frame(lazega, what="vertices")

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
lazega.s ~ edges

# CHUNK 4
summary.statistics(my.ergm.bern)
# ---
## edges 
##   115
# ---

# CHUNK 5
my.ergm <- formula(lazega.s ~ edges + kstar(2) 
   + kstar(3) + triangle)
summary.statistics(my.ergm)
# ---
##    edges   kstar2   kstar3 triangle 
##      115      926     2681      120
# ---

# CHUNK 6
my.ergm <- formula(lazega.s ~ edges 
   + gwesp(1, fixed=TRUE))
summary.statistics(my.ergm)
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
anova.ergm(lazega.ergm.fit)
# ---
## Analysis of Variance Table
## 
## Model 1: lazega.s ~ edges + gwesp(log(3), fixed = TRUE) + 
##     nodemain("Seniority") + nodemain("Practice") + 
##     match("Practice") + match("Gender") + 
##     match("Office")
##          Df Deviance Resid. Df Resid. Dev Pr(>|Chisq|)
## NULL                       630       0.00             
## Model 1:  7  -458.86       623     458.86    < 2.2e-16
##             
## NULL        
## Model 1: ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ---

# CHUNK 10
summary.ergm(lazega.ergm.fit)
# ---
## ==========================
## Summary of model fit
## ==========================
## 
## Formula: lazega.s ~ edges + gwesp(log(3), fixed = TRUE) + 
##     nodemain("Seniority") + nodemain("Practice") + 
##     match("Practice") + match("Gender") + 
##     match("Office")
## 
## Iterations:  20 
## 
## Monte Carlo MLE Results:
##                              Estimate Std. Error MCMC %
## edges                        -6.98047    0.72739      0
## gwesp.fixed.1.09861228866811  0.58967    0.08786      0
## nodecov.Seniority             0.02442    0.00675      0
## nodecov.Practice              0.39538    0.11013      0
## nodematch.Practice            0.76438    0.20055      0
## nodematch.Gender              0.72110    0.25167      0
## nodematch.Office              1.16155    0.19498      0
##                               p-value    
## edges                         < 1e-04 ***
## gwesp.fixed.1.09861228866811  < 1e-04 ***
## nodecov.Seniority            0.000321 ***
## nodecov.Practice             0.000357 ***
## nodematch.Practice           0.000152 ***
## nodematch.Gender             0.004308 ** 
## nodematch.Office              < 1e-04 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
##      Null Deviance: 873.4  on 630  degrees of freedom
##  Residual Deviance: 458.9  on 623  degrees of freedom
##  
## AIC: 472.9    BIC: 504    (Smaller is better.)
# ---

# CHUNK 11
gof.lazega.ergm <- gof(lazega.ergm.fit)

# CHUNK 12
par(mfrow=c(1, 3))
plot(gof.lazega.ergm)

# CHUNK 13
library(mixer)
setSeed(42)
fblog.sbm <- mixer(as.matrix(get.adjacency(fblog)),
                    qmin=2, qmax=15)

# CHUNK 14
fblog.sbm.output <- getModel(fblog.sbm)
names(fblog.sbm.output)
# ---
## [1] "q"         "criterion" "alphas"    "Pis"      
## [5] "Taus"
# ---

# CHUNK 15
fblog.sbm.output$q
# ---
## [1] 12
# ---

# CHUNK 16
fblog.sbm.output$alphas
# ---
##  [1] 0.15294139 0.13007188 0.12307831 0.05729167 
##  [5] 0.13581585 0.03123927 0.09967103 0.09795210 
##  [9] 0.01041667 0.02088946 0.12500738 0.01562500
# ---

# CHUNK 17
fblog.sbm.output$Taus[, 1:3]
# ---
##               [,1]         [,2]         [,3]
##  [1,] 9.999820e-01 9.162358e-04 9.999910e-01
##  [2,] 1.182601e-05 1.000000e-10 5.169635e-07
##  [3,] 4.702876e-06 9.990596e-01 8.427162e-06
##  [4,] 1.000000e-10 1.000000e-10 1.000000e-10
##  [5,] 1.094414e-06 1.000000e-10 5.707788e-09
##  [6,] 1.000000e-10 1.000000e-10 1.000000e-10
##  [7,] 3.451962e-07 2.418009e-05 4.619964e-08
##  [8,] 1.000000e-10 1.000000e-10 1.000000e-10
##  [9,] 1.000000e-10 1.000000e-10 1.000000e-10
## [10,] 1.000000e-10 1.000000e-10 1.000000e-10
## [11,] 4.531089e-09 1.000000e-10 1.000000e-10
## [12,] 1.000000e-10 1.000000e-10 1.000000e-10
# ---

# CHUNK 18
my.ent <- function(x) { -sum(x*log(x, 2)) }
apply(fblog.sbm.output$Taus[, 1:3], 2, my.ent)
# ---
## [1] 0.0003319527 0.0109735939 0.0001671334
# ---

# CHUNK 19
log(fblog.sbm.output$q, 2)
# ---
## [1] 3.584963
# ---

# CHUNK 20
summary(apply(fblog.sbm.output$Taus, 2, my.ent))
# ---
##      Min.   1st Qu.    Median      Mean   3rd Qu. 
## 0.0000000 0.0000000 0.0000003 0.0343200 0.0006172 
##      Max. 
## 1.0100000
# ---

# CHUNK 21
plot(fblog.sbm, classes=as.factor(V(fblog)$PolParty))

# CHUNK 22
summary(lazega)
# ---
## IGRAPH UN-- 36 115 -- 
## attr: name (v/c), Seniority (v/n), Status (v/n),
##   Gender (v/n), Office (v/n), Years (v/n), Age
##   (v/n), Practice (v/n), School (v/n)
# ---

# CHUNK 23
library(eigenmodel)
A <- get.adjacency(lazega, sparse=FALSE)
lazega.leig.fit1 <- eigenmodel_mcmc(A, R=2, S=11000,
   burn=10000)

# CHUNK 24
same.prac.op <- v.attr.lazega$Practice %o% 
   v.attr.lazega$Practice
same.prac <- matrix(as.numeric(same.prac.op 
   %in% c(1, 4, 9)), 36, 36)
same.prac <- array(same.prac,dim=c(36, 36, 1))

# CHUNK 25
lazega.leig.fit2 <- eigenmodel_mcmc(A, same.prac, R=2,
   S=11000,burn=10000)

# CHUNK 26
same.off.op <- v.attr.lazega$Office %o% 
   v.attr.lazega$Office
same.off <- matrix(as.numeric(same.off.op %in% 
   c(1, 4, 9)), 36, 36)
same.off <- array(same.off,dim=c(36, 36, 1))
lazega.leig.fit3 <- eigenmodel_mcmc(A, same.off,
    R=2, S=11000, burn=10000)

# CHUNK 27
lat.sp.1 <- 
   eigen(lazega.leig.fit1$ULU_postmean)$vec[, 1:2]
lat.sp.2 <- 
   eigen(lazega.leig.fit2$ULU_postmean)$vec[, 1:2]
lat.sp.3 <- 
   eigen(lazega.leig.fit3$ULU_postmean)$vec[, 1:2]

# CHUNK 28
colbar <- c("red", "dodgerblue", "goldenrod")
v.colors <- colbar[V(lazega)$Office]
v.shapes <- c("circle", "square")[V(lazega)$Practice]
v.size <- 3.5*sqrt(V(lazega)$Years)
plot(lazega, layout=lat.sp.1, vertex.color=v.colors,
   vertex.shape=v.shapes, vertex.size=v.size)

# CHUNK 29
apply(lazega.leig.fit1$L_postsamp, 2, mean)
# ---
## [1] 0.2603655 1.0384032
# ---
apply(lazega.leig.fit2$L_postsamp, 2, mean)
# ---
## [1]  0.8898394 -0.1156671
# ---
apply(lazega.leig.fit3$L_postsamp, 2, mean)
# ---
## [1] 0.5970403 0.3112896
# ---

# CHUNK 30
perm.index <- sample(1:630)
nfolds <- 5
nmiss <- 630/nfolds
Avec <- A[lower.tri(A)]
Avec.pred1 <- numeric(length(Avec))

# CHUNK 31
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

# CHUNK 32
library(ROCR)
pred1 <- prediction(Avec.pred1, Avec)
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1, col="blue", lwd=3)

# CHUNK 33
perf1.auc <- performance(pred1, "auc")
slot(perf1.auc, "y.values")
# ---
## [[1]]
## [1] 0.8246264
# ---

