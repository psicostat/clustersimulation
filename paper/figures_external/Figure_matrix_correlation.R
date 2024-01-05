
#######################################
#######################################

set.seed(100100)
rm(list=ls())
library(fpc)
library(MASS)
library(cluster)
library(corrplot)
library(ggplot2)

hclust_opt<-function(x, k=5, alpha=0.05){
  k2<-cutree(hclust(dist(x),method = "complete"),2)
  if (dudahart2(x,k2,alpha=alpha)$cluster1) {
    if(is.matrix(x)) k=rep(1,dim(x)[1])
    if(!is.matrix(x)) k=rep(1,length(x))
  }
  if (!dudahart2(x,k2,alpha=alpha)$cluster1) {
    sill<-c(NA,rep(NA,k-1))
    for (i in 2:k){
      r<-cutree(hclust(dist(x),method = "complete"),i)
      ss <- silhouette(r, dist(x))
      sill[i]<-mean(ss[, 3])
    }  
    grouping <- cutree(hclust(dist(x),method = "complete"),which.max(sill[-1])+1)
  }
  list(nc=max(k),clust=grouping)
} 

kmeans_opt = function(data=NA, krange=1:5, alpha=0.05){
    # first perform duda-hart test on 2-cluster solution
    km2 = kmeans(data, centers=2)
    dh = dudahart2(data, clustering=km2$cluster, alpha=alpha)
    if(dh$cluster1 & 1%in%krange == TRUE){
        return(1)
    }else{ # test more clusters only if duda-hart test is significant
        sil = rep(-1,max(krange))
        for(i in krange[krange!=1]){
            km = kmeans(data, centers=i)
            # compute silhouette value for i-cluster solution
            silvalue = silhouette(km$cluster, 
                                  dist=dist(data, 
                                            method="euclidean"))[,"sil_width"]
            sil[i] = round(mean(silvalue), 8)
        }
        # best solution has maximum silhouette value
        return(which(sil==max(sil)))
    }
}

clustering = function(data=NA,method=c("pamk","hclust","kmeans"),k=5){
  if(method=="pamk"){
    cl = pamk(data=data,krange=1:k)
    return(cl$nc)
  }
  if(method=="hclust"){
    cl = hclust_opt(data)
    return(cl$nc)
  }
  if(method=="kmeans"){
    cl = kmeans_opt(data)
    return(cl)
  }
} 


#######################################
#######################################

# SIMULAZIONE

N = c(50,100,200,400,800,1600)
r = c(0)
loadingSpecific = c(0,1)
loadingGeneral = c(0,1)
sigmaRes = c(0.50, 2.00, 3.00)
method = c("kmeans")

tab = expand.grid(N=N,r=r,loadingSpecific=loadingSpecific,
                  loadingGeneral=loadingGeneral,sigmaRes=sigmaRes,method=method)
tab$cl5 = tab$cl4 = tab$cl3 = tab$cl2 = tab$cl1 = NA
head(tab)

 tab = tab[tab$loadingSpecific==0&tab$loadingGeneral==1&tab$sigmaRes==3.0 | 
               tab$loadingSpecific==1&tab$loadingGeneral==0&tab$sigmaRes==0.5, ]

niter = 1000
for(i in 1:nrow(tab)){
  kvector = rep(NA,niter)
  Sigma = matrix(tab$r[i],3,3)
  Sigma = Sigma + diag(3)*(1-tab$r[i])
  for(j in 1:niter){
    pop = mvrnorm(tab$N[i],mu=rep(0,dim(Sigma)[1]),Sigma=Sigma)
    A = pop[,1]; B = pop[,2]; C = pop[,3]
    g = rnorm(tab$N[i],0,1)
    d = data.frame(
      x1 = g*tab$loadingGeneral[i] +  A*tab$loadingSpecific[i] + rnorm(tab$N[i])*tab$sigmaRes[i],
      x2 = g*tab$loadingGeneral[i] +  A*tab$loadingSpecific[i] + rnorm(tab$N[i])*tab$sigmaRes[i],
      x3 = g*tab$loadingGeneral[i] +  B*tab$loadingSpecific[i] + rnorm(tab$N[i])*tab$sigmaRes[i],
      x4 = g*tab$loadingGeneral[i] +  B*tab$loadingSpecific[i] + rnorm(tab$N[i])*tab$sigmaRes[i],
      x5 = g*tab$loadingGeneral[i] +  C*tab$loadingSpecific[i] + rnorm(tab$N[i])*tab$sigmaRes[i],
      x6 = g*tab$loadingGeneral[i] +  C*tab$loadingSpecific[i] + rnorm(tab$N[i])*tab$sigmaRes[i]
    )
    kvector[j] = clustering(data=d,method=tab$method[i])
  }
  tab$cl1[i] = sum(kvector==1)
  tab$cl2[i] = sum(kvector==2)
  tab$cl3[i] = sum(kvector==3)
  tab$cl4[i] = sum(kvector==4)
  tab$cl5[i] = sum(kvector>=5)
  print(paste(i,"/",nrow(tab)))
  save(tab,file="Figure_matrix_correlation_WorkSpace.RData")
}
tab

save(tab,file="Figure_matrix_correlation_WorkSpace.RData")

#######################################
#######################################

#### PLOT RESULTS

r = 0
loadingSpecific = 0
loadingGeneral = 1
sigmaRes = 3

##################

# CORRELATION MATRIX PLOT

N = 1e6
Sigma = matrix(r,3,3)
Sigma = Sigma + diag(3)*(1-r)
pop = mvrnorm(N,mu=rep(0,dim(Sigma)[1]),Sigma=Sigma)
A = pop[,1]; B = pop[,2]; C = pop[,3]
g = rnorm(N,0,1)
d = data.frame(
    x1 = g*loadingGeneral +  loadingSpecific*A + rnorm(N)*sigmaRes,
    x2 = g*loadingGeneral +  loadingSpecific*A + rnorm(N)*sigmaRes,
    x3 = g*loadingGeneral +  loadingSpecific*B + rnorm(N)*sigmaRes,
    x4 = g*loadingGeneral +  loadingSpecific*B + rnorm(N)*sigmaRes,
    x5 = g*loadingGeneral +  loadingSpecific*C + rnorm(N)*sigmaRes,
    x6 = g*loadingGeneral +  loadingSpecific*C + rnorm(N)*sigmaRes
)

( corrplot(cor(d),method=c("color"),addCoef.col = "black",
         tl.cex=2.5, tl.col="black") 
)

##################

# CLUSTERING RESULTS PLOT

load("Figure_matrix_correlation_WorkSpace.RData")

x = tab[tab$r==r & tab$sigmaRes==sigmaRes & tab$method=="kmeans",]
x = reshape(x, varying=paste0("cl",1:5), v.names="freq", 
            timevar="clusters", direction="long")
x$clusters = factor(x$clusters,levels=5:1)
palette = c("black","deeppink","orange","deepskyblue1","coral")

ts = 18
( ggp = ggplot(x,aes(x=N,y=freq,fill=clusters,group=clusters))+
  geom_area(color="white",size=0.7)+ 
  scale_fill_manual(values=rev(palette))+
  theme(text=element_text(size=ts),title=element_text(size=ts*.75))+
  scale_x_continuous(trans="log",breaks=unique(x$N))+
  guides(fill=guide_legend(title="number of \ndetected\nclusters"))+
  ggtitle("Ground truth: 1 population (no clusters)")+
  ylab("Frequency (1000 iterations)")
)


##################

# Plot together

library(grid)
pdf("Figure_matrix_correlation.pdf",width=11,height=8)
vp.BottomRight <- viewport(height=unit(0.56, "npc"), width=unit(0.56, "npc"), 
                           just=c("left","top"), 
                           y=0.77, x=0.4)
par(mfrow=c(1,3))
corrplot(cor(d),method=c("color"),addCoef.col = "black",
         tl.cex=2, number.cex=1.7, tl.col="black",cl.pos="n") 
print(ggp, vp=vp.BottomRight)
dev.off()

#######################################
#######################################


