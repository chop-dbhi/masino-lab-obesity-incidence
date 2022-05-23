library(tidyverse)
library(poLCA)

obesityLCA2 <- read.table("/Users/campbelle2/Documents/lca_subset.csv", header = TRUE, sep = ",")

data3 <- obesityLCA2[, 8:61]
paste(names(data3), collapse = ",")

f = cbind(X1.ALL04, X1.EAR08, X1.EAR09, X1.EAR12, 
  X1.EYE11, X1.GAS03, X1.GAS07, X1.GSI03, X1.GSI05, X1.GSU09, 
  X1.GUR06, X1.NUR02, X1.NUR07, X1.NUR10, X1.NUR26, X1.REC02, X1.RES01, 
  X1.RES06, X1.SKN01, X1.SKN02,  X1.SKN09, X1.SKN11, X2.ALL03, 
  X2.ALL04, X2.CAR11, X2.EAR09, X2.END05, X2.EYE01, X2.GAS03, X2.GAS08, X2.GUR06, X2.MUS01, X2.MUS04, 
  X2.MUS15, X2.MUS17, X2.NUR01, X2.NUR02, X2.NUR07, X2.NUR10, 
  X2.NUR19, X2.NUR22, X2.NUR26, X2.PSY10, X2.PSY14, X2.RES01, 
  X2.RES06, X2.SKN02, X2.SKN04, X3.EAR09, X3.NUR07, X3.NUR10, 
  X3.NUR19, X3.NUR26, X3.RES06) ~ 1


data3 <- mutate_all(data3,function(x) x=x+1)
LCA2<-poLCA(f,data3,nclass=3,verbose=FALSE,nrep=10)

save(LCA2,file = "/Volumes/maltenform/Documents/obesityLCA2.RData")

ls()

plot(LCA2)

# generate a series of latent class models with one to seven classes:

k = 7
for (i in 1:k){
assign(paste("lc",i,sep=""),
poLCA(f,data3,nclass=i,maxiter=3000,
tol=1e-5,na.rm=FALSE,
nrep=10,verbose=TRUE,calc.se=TRUE))
}

tab.modfit <- data.frame(matrix(rep(999,7),nrow=1))
names(tab.modfit)<-c("log-likelihood","resid.df","BIC","aBIC","cAIC","likelihood-ratio","Entropy")

entropy.poLCA<- function(lc)
{ 
  K.j <- sapply(lc$probs,ncol)
  if(length(unique(K.j))==1) {
    fullcell <- expand.grid(data.frame(sapply(K.j,seq,from=1)))
  } else{
    fullcell <- expand.grid(sapply(K.j,seq,from=1))
  }
  P.c<- poLCA.predcell(lc,fullcell)
  return(-sum(P.c*log(P.c),na.rm=TRUE))
}
