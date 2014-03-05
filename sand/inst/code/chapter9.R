# SAND with R, chapter9.tex

# CHUNK 1
library(sand)
data(calldata)
names(calldata)
# ---
## [1] "Orig"    "Dest"    "DistEuc" "DistRd"  "O.GRP"  
## [6] "D.GRP"   "Flow"
# ---

# CHUNK 2
min.call <- min(calldata$Flow)
calldata$FlowCnt <- round(5 * calldata$Flow / min.call)

# CHUNK 3
W <- xtabs(FlowCnt ~ Orig + Dest, calldata)
g.cd <- graph.adjacency(W, weighted=TRUE)

# CHUNK 4
in.flow <- graph.strength(g.cd, mode="in")
out.flow <- graph.strength(g.cd, mode="out")
vsize <- sqrt(in.flow + out.flow) / 100
pie.vals <- lapply((1:vcount(g.cd)), 
   function(i) c(in.flow[i], out.flow[i]))
ewidth <- E(g.cd)$weight / 10^5
plot(g.cd, vertex.size=vsize, vertex.shape="pie",
   vertex.pie=pie.vals, edge.width=ewidth,
   edge.arrow.size=0.1)

# CHUNK 5
calldata$lFlowCnt <- log(calldata$FlowCnt, 10)
calldata$lO.GRP <- log(calldata$O.GRP, 10)
calldata$lD.GRP <- log(calldata$D.GRP, 10)
calldata$lDistRd <- log(calldata$DistRd, 10)

library(car)
scatterplotMatrix( ~ lFlowCnt + lO.GRP + lD.GRP + 
   lDistRd, data=calldata)

# CHUNK 6
formula.s <- FlowCnt ~ lO.GRP + lD.GRP + lDistRd

# CHUNK 7
formula.g <- FlowCnt ~ Orig + Dest + lDistRd

# CHUNK 8
gm.s <- glm(formula.s, family="poisson", data=calldata)
gm.g <- glm(formula.g, family="poisson", data=calldata)

# CHUNK 9
summary(gm.s)
# ---
## 
## Call:
## glm(formula = formula.s, family = "poisson", data = calldata)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -475.06   -54.16   -29.20    -2.09  1149.93  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.149e+01  5.394e-03   -2131   <2e-16 ***
## lO.GRP       1.885e+00  4.306e-04    4376   <2e-16 ***
## lD.GRP       1.670e+00  4.401e-04    3794   <2e-16 ***
## lDistRd     -2.191e+00  7.909e-04   -2770   <2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 45490237  on 991  degrees of freedom
## Residual deviance: 10260808  on 988  degrees of freedom
## AIC: 10270760
## 
## Number of Fisher Scoring iterations: 5
# ---

# CHUNK 10
gm.g$aic
# ---
## [1] 5466814
# ---
gm.s$aic
# ---
## [1] 10270760
# ---

# CHUNK 11
plot(calldata$lFlowCnt,log(gm.g$fitted.values,10),
   cex.lab=1.5,
   xlab=expression(Log[10](paste("Flow Volume"))),
   col="green", cex.axis=1.5, ylab="", ylim=c(2, 5.75))
mtext(expression(Log[10](paste("Fitted Value"))), 2,
   outer=T, cex=1.5, padj=1)
abline(0, 1, lwd=2, col="darkgoldenrod1")

# CHUNK 12
res <- residuals.glm(gm.g, type="response")
relres <- res/calldata$FlowCnt
lrelres <- log(abs(relres),10)
res.sgn <- (relres>=0)

plot(calldata$lFlowCnt[res.sgn], lrelres[res.sgn],
   xlim=c(0.5,5.75), ylim=c(-3.5,3.5),
   xlab=expression(Log[10](paste("Flow Volume"))),
   cex.lab=1.5, cex.axis=1.5, ylab="", col="lightgreen")
mtext(expression(Log[10](paste("Relative Error"))), 2,
   outer=T, cex=1.5, padj=1)
par(new=T)
plot(calldata$lFlowCnt[!res.sgn], lrelres[!res.sgn],
   xlim=c(0.5,5.75), ylim=c(-3.5, 3.5),
   xlab=expression(Log[10](paste("Flow Volume"))),
   cex.lab=1.5, cex.axis=1.5, ylab="", col="darkgreen")
mtext(expression(Log[10](paste("Relative Error"))), 2,
   outer=T, cex=1.5, padj=1)
abline(h=0, lwd=2, col="darkgoldenrod2")

# CHUNK 13
library(networkTomography)
data(bell.labs)

# CHUNK 14
g.bl <- graph.formula(fddi:switch:local:corp ++ Router)
plot(g.bl)

# CHUNK 15
B <- bell.labs$A
Z <- bell.labs$X
x <- bell.labs$Y

# CHUNK 16
library(lattice)
traffic.in <- c("dst fddi","dst switch",
   "dst local","dst corp")
traffic.out <- c("src fddi","src switch",
   "src local","src corp")
my.df <- bell.labs$df
my.df$t <- unlist(lapply(my.df$time, function(x) {
     hrs <- as.numeric(substring(x, 11, 12))
     mins <- as.numeric(substring(x, 14, 15))
     t <- hrs + mins/60
     return(t)}))

# Separate according to whether data
# are incoming or outgoing.
my.df.in <- subset(my.df, nme %in% traffic.in)
my.df.out <- subset(my.df, nme %in% traffic.out)

# Set up trellis plots for each case.
p.in <- xyplot(value / 2^10 ~ t | nme, data=my.df.in,
   type="l", col.line="goldenrod",
   lwd=2, layout=c(1,4),
   xlab="Hour of Day", ylab="Kbytes/sec")
p.out <- xyplot(value / 2^10 ~ t | nme, data=my.df.out,
   type="l", col.line="red",
   lwd=2, layout=c(1,4),
   xlab="Hour of Day", ylab="Kbytes/sec")

# Generate trellis plots.
print(p.in, position=c(0,0.5,1,1), more=TRUE)
print(p.out, position=c(0,0,1,0.5))

# CHUNK 17
B.full <- rbind(B, 2 - colSums(B))
write.table(format(B.full),
   row.names=F, col.names=F, quote=F)
# ---
## 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0
## 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0
## 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0
## 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1
## 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0
## 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0
## 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0
## 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1
# ---

# CHUNK 18
x.full <- Z %*% t(B.full)
tomo.fit <- tomogravity(x.full, B.full, 0.01)
zhat <- tomo.fit$Xhat

# CHUNK 19
nt <- nrow(Z); nf <- ncol(Z)
t.dat <- data.frame(z = as.vector(c(Z) / 2^10),
   zhat = as.vector(c(zhat) / 2^10),
   t <- c(rep(as.vector(bell.labs$tvec), nf)))

od.names <- c(rep("fddi->fddi", nt), 
   rep("fddi->local", nt),
   rep("fddi->switch", nt), rep("fddi->corp",nt),
   rep("local->fddi", nt), rep("local->local",nt),
   rep("local->switch", nt), rep("local->corp",nt),
   rep("switch->fddi", nt), rep("switch->local",nt),
   rep("switch->switch", nt), rep("switch->corp",nt),
   rep("corp->fddi", nt), rep("corp->local",nt),
   rep("corp->switch", nt), rep("corp->corp",nt))

t.dat <- transform(t.dat, OD = od.names)

xyplot(z~t | OD, data=t.dat,
   panel=function(x, y, subscripts){
     panel.xyplot(x, y, type="l", col.line="blue")
     panel.xyplot(t.dat$t[subscripts], 
     t.dat$zhat[subscripts], 
     type="l", col.line="green")
   }, as.table=T, subscripts=T, xlim=c(0,24),
   xlab="Hour of Day", ylab="Kbytes/sec")

