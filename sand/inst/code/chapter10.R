# SAND with R, chapter10.tex

# CHUNK 1
library(sand)
data(hc)

# CHUNK 2
head(hc)
# ---
##   Time ID1 ID2  S1  S2
## 1  140  15  31 MED ADM
## 2  160  15  22 MED MED
## 3  500  15  16 MED MED
## 4  520  15  16 MED MED
## 5  560  16  22 MED MED
## 6  580  16  22 MED MED
# ---

# CHUNK 3
ID.stack <- c(hc$ID1,hc$ID2)
Status.stack <- c(as.character(hc$S1),
   as.character(hc$S2))
my.t <- table(ID.stack,Status.stack)
v.status <- character(nrow(my.t))
for(i in (1:length(v.status))){
   v.status[i] <- names(which(my.t[i,]!=0))
 }
table(v.status)
# ---
## v.status
## ADM MED NUR PAT 
##   8  11  27  29
# ---

# CHUNK 4
status.t <- table(hc$S1,hc$S2)
status.t <- status.t + t(status.t)
diag(status.t) <- round(diag(status.t)/2)
status.t
# ---
##         ADM   MED   NUR   PAT
##   ADM   279   459  2596   441
##   MED   459  5660  1769  1471
##   NUR  2596  1769 12695  6845
##   PAT   441  1471  6845   209
# ---

# CHUNK 5
tmp.es <- paste(hc$S1,"-",hc$S2,sep="")
e.status <- character(dim(hc)[[1]])
e.status[tmp.es=="ADM-ADM"] <- "ADM-ADM"
e.status[tmp.es=="MED-MED"] <- "MED-MED"
e.status[tmp.es=="NUR-NUR"] <- "NUR-NUR"
e.status[tmp.es=="PAT-PAT"] <- "PAT-PAT"
e.status[(tmp.es=="ADM-MED") | 
   (tmp.es=="MED-ADM")] <- "ADM-MED"
e.status[(tmp.es=="ADM-NUR") |
   (tmp.es=="NUR-ADM")] <- "ADM-NUR"
e.status[(tmp.es=="ADM-PAT") | 
   (tmp.es=="PAT-ADM")] <- "ADM-PAT"
e.status[(tmp.es=="MED-NUR") | 
   (tmp.es=="NUR-MED")] <- "MED-NUR"
e.status[(tmp.es=="MED-PAT") | 
   (tmp.es=="PAT-MED")] <- "MED-PAT"
e.status[(tmp.es=="NUR-PAT") | 
   (tmp.es=="PAT-NUR")] <- "NUR-PAT"

my.hc <- data.frame(Time = hc$Time/(60*60),
                      ID1 = hc$ID1,
                      ID2 = hc$ID2,
                      Status = e.status)

library(lattice)
histogram(~Time|Status, data=my.hc, xlab="Hours", 
   layout=c(5,2))

# CHUNK 6
vids <- sort(unique(c(hc$ID1, hc$ID2)))
g.week <- graph.data.frame(hc[, c("ID1", "ID2", 
   "Time")], vertices=data.frame(vids),
   directed=FALSE)
E(g.week)$Time <- E(g.week)$Time  / (60 * 60)

# CHUNK 7
g.week
# ---
## IGRAPH UN-- 75 32424 -- 
## + attr: name (v/c), Time (e/n)
# ---

# CHUNK 8
status <- unique(rbind(data.frame(id=hc$ID1,
   status=hc$S1), data.frame(id=hc$ID2, status=hc$S2)))
V(g.week)$Status <- 
   as.character(status[order(status[,1]),2])

# CHUNK 9
E(g.week)$weight <- 1
g.week.wgtd <- simplify(g.week)
g.week.wgtd
# ---
## IGRAPH UNW- 75 1139 -- 
## + attr: name (v/c), Status (v/c), weight (e/n)
# ---

# CHUNK 10
is.simple(g.week.wgtd)
# ---
## [1] TRUE
# ---

# CHUNK 11
summary(E(g.week.wgtd)$weight)
# ---
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    3.00    8.00   28.47   23.00 1059.00
# ---

# CHUNK 12
g.week.96 <- subgraph.edges(g.week,
   E(g.week)[Time <= 96])

# CHUNK 13
g.sl12 <- lapply(1:8, function(i) {
   g <- subgraph.edges(g.week, 
                       E(g.week)[Time > 12*(i-1) & 
                                 Time <= 12*i],
                       delete.vertices=FALSE)
   simplify(g)
 })

# CHUNK 14
sapply(g.sl12,vcount)
# ---
## [1] 75 75 75 75 75 75 75 75
# ---

# CHUNK 15
sapply(g.sl12,ecount)
# ---
## [1] 179 294 257 282 265 314 197 305
# ---

# CHUNK 16
library(networkDynamic)

# CHUNK 17
hc.spls <- cbind((hc$Time-20)/(60*60),
                   hc$Time/(60*60),
                   hc$ID1,hc$ID2)
hc.dn <- networkDynamic(edge.spells=hc.spls)

# CHUNK 18
is.active(hc.dn,onset=0,terminus=1,e=c(1))
# ---
## [1] TRUE
# ---
is.active(hc.dn,onset=1,terminus=2,e=c(1))
# ---
## [1] FALSE
# ---

# CHUNK 19
get.edge.activity(hc.dn,e=c(1))
# ---
## [[1]]
##            [,1]       [,2]
## [1,] 0.03333333 0.03888889
# ---

# CHUNK 20
get.edge.activity(hc.dn,e=c(10))
# ---
## [[1]]
##           [,1]       [,2]
## [1,]  0.800000  0.8055556
## [2,]  1.355556  1.3611111
## [3,]  1.505556  1.5111111
## [4,] 24.894444 24.9055556
## [5,] 25.005556 25.0166667
## [6,] 25.388889 25.3944444
## [7,] 25.500000 25.5055556
# ---

# CHUNK 21
g.sl12.dN <- get.networks(hc.dn,start=0,end=96,
                           time.increment=12)

# CHUNK 22
hc.dn.df <- as.data.frame(hc.dn)
names(hc.dn.df)
# ---
## [1] "onset"             "terminus"         
## [3] "tail"              "head"             
## [5] "onset.censored"    "terminus.censored"
## [7] "duration"          "edge.id"
# ---

# CHUNK 23
summary(hc.dn.df$duration)
# ---
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.005556 0.005556 0.005556 0.012830 0.011110 1.089000
# ---

# CHUNK 24
detach(package:networkDynamic)
l = layout.fruchterman.reingold(g.week.wgtd)
v.cols <- character(75)
v.cols[V(g.week.wgtd)$Status=="ADM"] <- "yellow"
v.cols[V(g.week.wgtd)$Status=="MED"] <- "blue"
v.cols[V(g.week.wgtd)$Status=="NUR"] <- "green"
v.cols[V(g.week.wgtd)$Status=="PAT"] <- "black"
plot(g.week.wgtd, layout=l, vertex.size=3,
    edge.width=2*(E(g.week.wgtd)$weight)/100,
    vertex.color=v.cols,vertex.label=NA,)

# CHUNK 25
opar <- par()
par(mfrow=c(2,4),
     mar=c(0.5, 0.5, 0.5, 0.5), 
     oma=c(0.5, 1.0, 0.5, 0))
for(i in (1:8)) {
   plot(g.sl12[[i]], layout=l, vertex.size=5,
     edge.width=2*(E(g.week.wgtd)$weight)/1000,
     vertex.color=v.cols,vertex.label=NA)
   title(paste(12*(i-1),"to",12*i,"hrs"))
 }
par(opar)

# CHUNK 26
# Establish colors for edge status.
tmp.es <- paste(v.status[hc.dn.df$tail],"-",
                 v.status[hc.dn.df$head],sep="")
mycols <- numeric(nrow(hc.dn.df))
mycols[tmp.es=="ADM-ADM"] <- 1
mycols[tmp.es=="MED-MED"] <- 2
mycols[tmp.es=="NUR-NUR"] <- 3
mycols[tmp.es=="PAT-PAT"] <- 4
mycols[(tmp.es=="ADM-MED") | (tmp.es=="MED-ADM")] <- 5
mycols[(tmp.es=="ADM-NUR") | (tmp.es=="NUR-ADM")] <- 6
mycols[(tmp.es=="ADM-PAT") | (tmp.es=="PAT-ADM")] <- 7
mycols[(tmp.es=="MED-NUR") | (tmp.es=="NUR-MED")] <- 8
mycols[(tmp.es=="MED-PAT") | (tmp.es=="PAT-MED")] <- 9
mycols[(tmp.es=="NUR-PAT") | (tmp.es=="PAT-NUR")] <- 10
my.palette <- rainbow(10)
# Produce plot.
ne <- max(hc.dn.df$edge.id)
max.t <- max(hc.dn.df$terminus)
plot(c(0,max.t),c(0,ne),ann=F,type='n')
segments(hc.dn.df$onset,hc.dn.df$edge.id,
   hc.dn.df$terminus,hc.dn.df$edge.id,
   col=my.palette[mycols])
title(xlab="Time (hours)",
   ylab="Interacting Pair 
   (Ordered by First Interaction)")
abline(v=c(11,35,59,83),lty="dashed",lw=2,
   col="lightgray")
# Add legend to plot.
status.pairs <- c("ADM-ADM","MED-MED","NUR-NUR",
   "PAT-PAT", "ADM-MED","ADM-NUR","ADM-PAT",
    "MED-NUR", "MED-PAT","NUR-PAT")
legend(7,1170,status.pairs,
        text.col=my.palette[(1:10)],cex=0.75)

# CHUNK 27
all.deg <- sapply(g.sl12,degree)
sl.lab<- unlist(lapply(1:8, function(i) 
   paste(12*(i-1), "-", 12*i, "hrs", sep="")))
deg.df <- data.frame(Degree=as.vector(all.deg),
   Slice = rep(sl.lab,each=75),
   Status = rep(V(g.week)$Status, times=8))
library(ggplot2)
p = qplot(factor(Degree), data=deg.df,
           geom="bar",fill=Status)
p+facet_grid(Slice~.) + xlab("Degree") + ylab("Count")

# CHUNK 28
top.deg <- lapply(1:8,function(i) {
   all.deg[,i][rank(all.deg[,i])>=70]
 })
table(unlist(lapply(1:8,function(i) 
   as.numeric(names(top.deg[[i]])))))
# ---
##  1  5  7  8 10 11 13 15 17 19 21 22 23 24 25 26 27 29 31 
##  2  2  4  1  1  2  2  4  3  3  2  1  3  2  1  2  2  2  1 
## 34 36 37 63 64 
##  1  1  2  1  2
# ---

# CHUNK 29
V(g.week)$Status[c(7,15)]
# ---
## [1] "NUR" "MED"
# ---

# CHUNK 30
all.str <- sapply(g.sl12,graph.strength)
all.r <- all.str/all.deg
round(all.r[c(7,15),],digits=2)
# ---
##     [,1]  [,2] [,3]  [,4]  [,5]  [,6]  [,7]  [,8]
## 7   2.00 25.79 11.1 32.73 14.20 33.19  8.33 37.34
## 15 29.71 26.33 17.0 12.48 19.27 15.30 19.40 12.93
# ---

# CHUNK 31
summary(c(all.r))
# ---
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    1.00    5.00   10.43   12.06   15.73   47.35     261
# ---

# CHUNK 32
sp.len <- lapply(1:8, function(i) {
   spl <- shortest.paths(g.sl12[[i]],v=c(7,15),
                         to=V(g.sl12[[i]]),
                         weights=NA)
   spl[spl==Inf] <- NA
   spl
 })
ave.spl <- sapply(1:8,function(i) 
                   apply(sp.len[[i]],1,mean,na.rm=T))
round(ave.spl,digits=2)
# ---
##    [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## 7  3.05 1.27 1.79 1.12 1.80 1.33 2.00 1.24
## 15 1.72 1.51 2.48 1.35 1.36 1.26 1.61 1.36
# ---

# CHUNK 33

sapply(g.sl12,diameter)
# ---
## [1]  9  8 26 28 10 10 10 10
# ---

# CHUNK 34
round(sapply(g.sl12,average.path.length),digits=2)
# ---
## [1] 2.12 1.70 1.81 1.67 1.79 1.70 1.91 1.78
# ---

