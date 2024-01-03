
###################################################

rm(list=ls())
library(ggplot2)
library(semTools)
library(mclust)
library(furrr)
library(ggExtra)
library(gridExtra)

sim_best_GMM = function(N=NA,p=NA,skewness=NA,G=1:5){
    Sigma = diag(p)
    pop = data.frame(mvrnonnorm(n=N, mu=rep(0,p), Sigma=Sigma, skewness=skewness))
    best_G = Mclust(pop,G=G,verbose=F)$G
    return(best_G)
}

###################################################

# SIMULATE DATA

set.seed(0)

# define parameters
N = rev(c(50,100,200,400,800,1600))
skewness = c(.5)
p = c(4)

# initialize table for results
tab = expand.grid(N=N, p=p, skewness=skewness)
tab$cl5 = tab$cl4 = tab$cl3 = tab$cl2 = tab$cl1 = NA

# run iterations for all table rows
niter = 1000
for(i in 1:nrow(tab)){
    print(i)
    plan(multisession(workers=7))
    
    gmmvector = rep(NA,niter)
    res = future_pmap(data.frame(N=rep(tab$N[i],niter), p=rep(tab$p[i],niter), skewness=rep(tab$skewness[i],niter)), 
                      sim_best_GMM,
                      .options = furrr_options(seed = TRUE),
                      .progress = TRUE)
    gmmvector = unlist(res)
    tab$cl1[i] = sum(gmmvector==1)
    tab$cl2[i] = sum(gmmvector==2)
    tab$cl3[i] = sum(gmmvector==3)
    tab$cl4[i] = sum(gmmvector==4)
    tab$cl5[i] = sum(gmmvector>=5)
    save(tab,file="Figure_modest_skewness_WorkSpace.RData")
}

# see results
tab


###################################################

# PLOT RESULTS

load("Figure_modest_skewness_WorkSpace.RData")

# select parameters
p = 4
skewness = 0.5

# prepara visual data
x = tab[tab$p==p & tab$skewness==skewness, ]
x = reshape(x, varying=paste0("cl",1:5), v.names="freq", 
            timevar="clusters", direction="long")
x$clusters = factor(x$clusters,levels=5:1)
palette = c("black","deeppink","orange","deepskyblue1","coral")

# ggplot of skewed distribution
ts = 16
df = data.frame(mvrnonnorm(n=1e6, mu=rep(0,p), Sigma=diag(p), skewness=skewness))
(gghist = ggplot(df)+
    ggtitle("A) Skewed distribution \n(skew. = 0.5)")+
    theme(text=element_text(size=ts),title=element_text(size=ts*.80))+
    geom_histogram(aes(x=X1,y=after_stat(density)),fill="blue",alpha=.4,color="black",binwidth=.40)+
    coord_cartesian(xlim=c(quantile(df$X1,.000),quantile(df$X1,.9995)))
)

# ggplot of clustering results
ts = 16
( ggclust = ggplot(x,aes(x=N,y=freq,fill=clusters,group=clusters))+
        geom_area(color="white",size=0.7)+ 
        scale_fill_manual(values=rev(palette))+
        theme(text=element_text(size=ts),title=element_text(size=ts*.80))+
        scale_x_continuous(trans="log",breaks=unique(x$N))+
        guides(fill=guide_legend(title="number of \ndetected\nclusters"))+
        ggtitle("B) Detected clusters (GMM)\n (with 4 orthogonal variables)")+
        ylab("Frequency (1000 iterations)")
)

png("Figure_modest_skewnessX.png",width=3200,height=1800,res=350)
grid.arrange(gghist,ggclust,
             layout_matrix=matrix(c(1,2,2),nrow=1),
             ncol=2)
dev.off()



###################################################


