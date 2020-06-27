# SAND with R, chapter10.tex

# CHUNK 1
library(sand)
data(strike)

# CHUNK 2
summary(strike)
# ---
## IGRAPH 2669265 U--- 24 38 -- 
## + attr: names (v/c), race (v/c)
# ---

# CHUNK 3
table(V(strike)$race)
# ---
## OE YE YS 
## 11  9  4
# ---

# CHUNK 4
# Create a triangle vertex shape
mytriangle <- function(coords, v=NULL, params) {
   vertex.color <- params("vertex", "color")
   if (length(vertex.color) != 1 && !is.null(v)) {
     vertex.color <- vertex.color[v]
   }
   vertex.size <- 1/200 * params("vertex", "size")
   if (length(vertex.size) != 1 && !is.null(v)) {
     vertex.size <- vertex.size[v]
   }
   
   symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
           stars=cbind(vertex.size, vertex.size, vertex.size),
           add=TRUE, inches=FALSE)
}
add_shape("triangle", clip=shapes("circle")$clip,
           plot=mytriangle)

# CHUNK 5
V(strike)[V(strike)$race=="YS"]$shape <- "circle"
V(strike)[V(strike)$race=="YE"]$shape <- "square"
V(strike)[V(strike)$race=="OE"]$shape <- "triangle"

# CHUNK 6
nv <- vcount(strike)
z <- numeric(nv)
z[c(5,15,21,22)] <- 1
V(strike)$color <- rep("white",nv)
V(strike)[z==1]$color <- "red3"

# CHUNK 7
set.seed(42)
my.dist <- c(rep(1.8,4),rep(2.2,9),rep(2,11))
l <- layout_with_kk(strike)
plot(strike,layout=l,vertex.label=V(strike)$names,
      vertex.label.degree=-pi/3,
      vertex.label.dist=my.dist)

# CHUNK 8
V(strike)[z==1]$names
# ---
## [1] "Bob"     "Norm"    "Sam*"    "Wendle*"
# ---

# CHUNK 9
rank(-betweenness(strike))[z==1]
# ---
## [1]  1  2  4 21
# ---
rank(-closeness(strike))[z==1]
# ---
## [1]  1.0  2.0  6.5 22.0
# ---

# CHUNK 10
A <- as_adjacency_matrix(strike)
I.ex.nbrs <- as.numeric(z%*%A > 0)
V(strike)[z*I.ex.nbrs==1]$names
# ---
## [1] "Bob"     "Norm"    "Sam*"    "Wendle*"
# ---

# CHUNK 11
V(strike)[(1-z)*I.ex.nbrs==1]$names
# ---
##  [1] "Alejandro" "Mike"      "Ike"       "Hal"      
##  [5] "John"      "Lanny"     "Ozzie"     "Paul"     
##  [9] "Vern"      "Xavier"    "Ultrecht"
# ---

# CHUNK 12
V(strike)[z*(1-I.ex.nbrs)==1]$names
# ---
## character(0)
# ---

# CHUNK 13
V(strike)[(1-z)*(1-I.ex.nbrs)==1]$names
# ---
## [1] "Domingo" "Carlos"  "Eduardo" "Gill"    "Frank"  
## [6] "Karl"    "Quint"   "Russ"    "Ted"
# ---

# CHUNK 14
O.c11 <- 10.0; O.c10 <- 7; O.c01 <- 5; O.c00 <- 1.0

# CHUNK 15
c(O.c11,O.c10,O.c01)-O.c00
# ---
## [1] 9 6 4
# ---

# CHUNK 16
# Initialize
set.seed(41)
m <- 4  # Number of representatives
n <- 10000 # Number of Monte Carlo trials
I11 <- matrix(,nrow=nv,ncol=n)
I10 <- matrix(,nrow=nv,ncol=n)
I01 <- matrix(,nrow=nv,ncol=n)
I00 <- matrix(,nrow=nv,ncol=n)
 
# Monte Carlo sampling
for(i in 1:n){
   z <- rep(0,nv)
   reps.ind <- sample((1:nv),m,replace=FALSE)
   z[reps.ind] <- 1
   reps.nbrs <- as.numeric(z%*%A > 0)
   I11[,i] <- z*reps.nbrs
   I10[,i] <- z*(1-reps.nbrs)
   I01[,i] <- (1-z)*reps.nbrs
   I00[,i] <- (1-z)*(1-reps.nbrs)
}

# CHUNK 17
I11.11 <- I11%*%t(I11)/n
I10.10 <- I10%*%t(I10)/n
I01.01 <- I01%*%t(I01)/n
I00.00 <- I00%*%t(I00)/n

# CHUNK 18
# Plot c00
names.w.space <- paste(V(strike)$names," ",sep="")
my.cex.x <- 0.75; my.cex.y <- 0.75
image(I00.00, zlim=c(0,0.7), xaxt="n", yaxt="n", 
       col=cm.colors(16))
mtext(side=1, text=names.w.space,at=seq(0.0,1.0,(1/23)), 
       las=3, cex=my.cex.x)
mtext(side=2, text=names.w.space,at=seq(0.0,1.0,1/23), 
       las=1, cex=my.cex.y)
mtext(side=3,text=expression("No Exposure"~(c["00"])), 
       at=0.5, las=1) 
# Add lines to differentiate groups.
u <- 1/23; uo2 <- 1/46
xmat <- cbind(rep(3*u+uo2,2),rep(12*u+uo2,2))
ymat <- cbind(c(0-uo2,1+uo2),c(0-uo2,1+uo2))
matlines(xmat,ymat, lty=1, lw=1, col="black")
matlines(ymat,xmat, lty=1, lw=1, col="black")

# CHUNK 19
z <- rep(0,nv)
z[c(5,15,21,22)] <- 1
reps.nbrs <- as.numeric(z%*%A > 0)
c11 <- z*reps.nbrs
c10 <- z*(1-reps.nbrs)
c01 <- (1-z)*reps.nbrs
c00 <- (1-z)*(1-reps.nbrs)
Obar.c11 <- O.c11*mean(c11/diag(I11.11))
Obar.c10 <- O.c10*mean(c10/diag(I10.10))
Obar.c01 <- O.c01*mean(c01/diag(I01.01))
Obar.c00 <- O.c00*mean(c00/diag(I00.00))
print(c(Obar.c11,Obar.c10,Obar.c01)-Obar.c00)
# ---
## [1] 23.5824874 -0.7973247  6.0490543
# ---

# CHUNK 20
set.seed(42)
n <- 10000
Obar.c11 <- numeric()
Obar.c10 <- numeric()
Obar.c01 <- numeric()
Obar.c00 <- numeric()
for(i in 1:n){
   z <- rep(0,nv)
   reps.ind <- sample((1:nv),m,replace=FALSE)
   z[reps.ind] <- 1
   reps.nbrs <- as.numeric(z%*%A > 0)
   c11 <- z*reps.nbrs
   c10 <- z*(1-reps.nbrs)
   c01 <- (1-z)*reps.nbrs
   c00 <- (1-z)*(1-reps.nbrs)
   Obar.c11 <- c(Obar.c11, O.c11*mean(c11/diag(I11.11)))
   Obar.c10 <- c(Obar.c10, O.c10*mean(c10/diag(I10.10)))
   Obar.c01 <- c(Obar.c01, O.c01*mean(c01/diag(I01.01)))
   Obar.c00 <- c(Obar.c00, O.c00*mean(c00/diag(I00.00)))
}
ACE <- list(Obar.c11-Obar.c00, Obar.c10-Obar.c00, 
             Obar.c01-Obar.c00)

# CHUNK 21
print(sapply(ACE,mean)- 
         c(O.c11-O.c00, O.c10-O.c00, O.c01-O.c00))
# ---
## [1]  -0.04493342  0.03896578  0.03712047
# ---

# CHUNK 22
print(sapply(ACE,sd))
# ---
## [1] 8.994901 3.752037 1.620195
# ---

# CHUNK 23
sapply(ACE,sd)/c(9,6,4)
# ---
## [1] 0.9994335 0.6253395 0.4050487
# ---

