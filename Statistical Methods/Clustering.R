ibrary(lubridate)
library(dplyr)
library(Matrix)
library(dendextend)
library(tidyverse)
library(factoextra)
library(cluster)

getwd()

DJ1 = read.csv("DJ30returns.csv")

DJuse = DJ1[-(1:5),]


DJuse = DJuse[1201:2400,]

head(DJuse)

DJuse = mutate( DJuse, Date = as_date( X, format = "%d/%Om/%y" ) ) %>% dplyr::select( -X ) %>%
  mutate_if( is.character, as.numeric )
summary(DJuse) 

DJuse_nodate = t(DJuse[,0:30])

dim(DJuse_nodate)

DJ_cov = cov(DJuse_nodate)

dim(DJ_cov)

rankMatrix(DJ_cov)

eigens = eigen(DJ_cov)$values

plot(x = seq(1:30),
     y = eigens[1:30], 
     type = 'o',
     ylab='EigenValue', xlab='Index')

##(b)##

DJ.clus.out = kmeans(
  DJuse_nodate,centers = 2,
  nstart=25)

names(DJ.clus.out)

DJ.clus.out$size
DJ.clus.out$cluster


##########################################
DJclus = data.frame()

for (k in sequence(from = 2,11, by=1)) {
  
  DJ.clus = kmeans(
    t(DJuse_nodate), centers = k,
    nstart=25,iter.max=50)
  
  clustered.df = data.frame(kk=k,
                            clus_label = DJ.clus$cluster)
  
  DJclus = rbind(DJclus, clustered.df)
  
  lol=xtabs( ~ clus_label + kk, data = DJclus) 
  
}



set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(DJuse_nodate, k, nstart = 25 )$tot.withinss
}

bss <- function(k) {
  kmeans(DJuse_nodate, k, nstart = 25 )$betweenss
}

totss <- function(k) {
  kmeans(DJuse_nodate, k, nstart = 25 )$totss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 2:12

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
bss_values <- map_dbl(k.values, bss)
totss_values <- map_dbl(k.values, totss)

plot(x=k.values,y= wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters",
     ylab="Within-cluster variability W")

plot(x=k.values,y= bss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters",
     ylab="Between-cluster variability B")


plot(x=k.values,y= totss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters",
     ylab="Total sum of squares")


###########Hierarchial#######3


pc.DJ = prcomp(DJuse_nodate)$x[,1:2]

autoplot(prcomp(DJuse_nodate),main="Score Plot",label=TRUE)



DJ.dendro = as.dendrogram(hclust(dist(t(DJuse_nodate), method = "euclidean"), method = 'complete'))

plot( DJ.dendro,main="Hierarchical clustering of daily returns",sub='Based on complete linkage and Euclidean distance')





dend_classes <- cutree((DJ.dendro), k = 1:12)

dend_classes12.fp = as.data.frame( dend_classes ) %>% pivot_longer(cols = 1:12,
                                                                   names_to = "max_clust", values_to = "cluster")

conf = xtabs( ~ cluster + max_clust,dend_classes12.fp)
conf = as.data.frame.matrix(conf,header='F')

#####3

wine <- read.csv("wine.csv", header=FALSE)

wine <- wine[,2:14]

set.seed(2315151)
wineclus = data.frame()

for (k in (1:10)) {
  
  wine.clus = kmeans(
    wine, centers = k,
    nstart=50)
  
  clustered.df = data.frame(kk=k,
                            clus_label = wine.clus$cluster)
  
  wineclus = rbind(wineclus, clustered.df)
  lol=xtabs( ~ clus_label + kk, data = wineclus) 
}

##Lab8 Q3##

wine <- read.csv("wine.csv", header=FALSE)

wine <- wine[,2:14]

wine_scaled <- scale(wine)

set.seed(2315151)
wineclus = data.frame()

for (k in (2:9)) {
  
  wine.clus = kmeans(
    wine_scaled, centers = k,
    nstart=25)
  
  clustered.df = data.frame(kk=k,
                            clus_label = wine.clus$cluster)
  
  wineclus = rbind(wineclus, clustered.df)
  lol=xtabs( ~ clus_label + kk, data = wineclus) 
}

####(d)#####
library(fpc)
library(clusterSim)
wine <- read.csv("wine.csv", header=FALSE)

wine <- wine[,2:14]

set.seed(2315151)
wineclus1 = data.frame()

for (k in (1:10)) {
  
  wine.clus = kmeans(
    wine, centers = k,
    nstart=50)
  
  
  clustered.df = data.frame(kk=k,
                            CH = calinhara(wine,wine.clus$cluster) )
  
  wineclus1 = rbind(wineclus1, clustered.df)
}

wineclus = data.frame()
for (k in (1:10)) {
  
  wine.clus1 = kmeans(
    wine, centers = k,
    nstart=50)
  
  wine.clus2 = kmeans(
    wine, centers = k+1,
    nstart=50)
  
  wine.clus3 = kmeans(
    wine, centers = k+2,
    nstart=50)
  
  Diff1 = ((k)**(2/13)*wine.clus1$tot.withinss) - ((k+1)**(2/13)*wine.clus2$tot.withinss)
  
  Diff2 = ((k+1)**(2/13)*wine.clus2$tot.withinss) - ((k+2)**(2/13)*wine.clus3$tot.withinss)
  
  kl = abs(Diff1/Diff2)
  
  clustered.df = data.frame(kk=k+1,
                            KL = kl,
                            Diffk = Diff1) 
  
  wineclus = rbind(wineclus, clustered.df)
}

KL = wineclus

k = 2
wine.clus1 = kmeans(
  wine, centers = k-1,
  nstart=50)
wine.clus2 = kmeans(
  wine, centers = k,
  nstart=50)
wine.clus3 = kmeans(
  wine, centers = k+1,
  nstart=50)

diff1 = ((k-1)**(2/13)*wine.clus1$tot.withinss) - (k**(2/13)*wine.clus2$tot.withinss)
diff2 = ((2)**(2/13)*wine.clus2$tot.withinss) - (3**(2/13)*wine.clus3$tot.withinss)

abs(diff1/diff2)

wine.clus = kmeans(
  wine, centers = 3,
  nstart=50)

(wine.clus$betweenss/2)/(wine.clus$tot.withinss/(178-3))



wineclus2 = data.frame()
for (k in (1:10)) {
  
  wine.clus1 = kmeans(
    wine, centers = k,
    nstart=50)
  
  wine.clus2 = kmeans(
    wine, centers = k+1,
    nstart=50)
  
  wine.clus3 = kmeans(
    wine, centers = k+2,
    nstart=50)
  
  clall=cbind(wine.clus1$cluster,wine.clus2$cluster,wine.clus3$cluster)
  
  kl = index.KL(wine,clall)
  
  clustered.df = data.frame(kk=k+1,
                            KL = kl) 
  
  wineclus2 = rbind(wineclus2, clustered.df)
}

wineclus3 = data.frame()
for (k in (1:10)) {
  
  wine.clus1 = kmeans(
    wine, centers = k,
    nstart=50)
  
  wine.clus2 = kmeans(
    wine, centers = k+1,
    nstart=50)
  
  wv = wine.clus1$tot.withinss / wine.clus2$tot.withinss
  
  clustered.df = data.frame(kk=k,
                            WV = wv) 
  
  wineclus3 = rbind(wineclus3, clustered.df)
}

yeet = data.frame('kk'=1, 'KL'='', 'Diffk'='')
new=rbind(yeet,KL[1:9,])

overall = merge(new, wineclus1)
overall = merge(overall, wineclus3)
overall[1,2] <- NaN
overall[1,3] = NaN
overall[1,4] = NaN

plot(main='KL vs # of clusters',data=overall, y = overall$KL, x = overall$kk, ylab='KL',xlab='k')
#lines(data=overall, y = overall$Diffk, x = overall$kk)
plot(main='KL vs # of clusters',data=overall, y = overall$KL, x = overall$kk, ylab='KL',xlab='k')
plot(main='CH vs # of clusters',data=overall, y = overall$CH, x = overall$kk, ylab='CH',xlab='k')
plot(main='WV vs # of clusters',data=overall, y = overall$WV, x = overall$kk, ylab='WV',xlab='k')
abline(h=1.2,col='red')
abline(h=1.5,col='red')




mm = kmeans(wine,centers=10,nstart =50)
B=mm$betweenss
W=mm$tot.withinss
k=10
n=178
(B/(k-1))/(W/(n-k))
