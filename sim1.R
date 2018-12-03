rm(list=ls())
setwd("/Volumes/GoogleDrive/My Drive/Dropbox/Research/Dr. Daeyoung Kim/Two_response/R code")
library(MASS)
source("subcopula_measure.R")
source("check_log_odds.R")
source("make_3_way_table.R")

mu<-rep(0,3)
Sigma <- matrix(c(1,0.25,0.5,
                  0.25,1,0.5,
                  0.5,0.5,1),3,3)

Sigma11<-Sigma[1:2,1:2]
Sigma12<-as.matrix(Sigma[1:2,3])
Sigma21<-t(as.matrix(Sigma[3,1:2]))
Sigma22<-Sigma[3,3]

dim(Sigma12)
dim(Sigma21)

Sigma11-Sigma12%*%Sigma22^(-1)%*%Sigma21

B<-1000#num of simulation runs
meanvec<-rep(0,B)
medianvec<-rep(0,B)
minvec<-rep(0,B)
maxvec<-rep(0,B)

for(b in 1:B){
cat("b=",b,"\n")
  
sim.data<-mvrnorm(n = 10000, mu, Sigma)

colnames(sim.data)<-c("yy","zz","xx")
sim.data<-as.data.frame(sim.data)



freq.tb<-fretabfun(data_c=sim.data,catgnum_row=5,catgnum_col=5,catgnum_cub=5)

#freq.tb<-freq.tb/sum(freq.tb)
freq.tb


#lapply(1:dim(freq.tb)[3],odd.ratio,freq.3d = freq.tb)
res<-lapply(1:dim(freq.tb)[3],globe.odd.ratio,freq.3d = freq.tb)
res1<-unlist(res)
res2<-res1[res1!=0]

meanvec[b]<-mean(res2)
medianvec[b]<-median(res2)
minvec[b]<-min(res2)
maxvec[b]<-max(res2)
}

pdf(file="plot10k.pdf")
par(mfrow=c(2,2))
hist(meanvec)
hist(medianvec)
hist(maxvec)
hist(minvec)
dev.off()
