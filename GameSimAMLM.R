library(tseries)
nobs=125
trials=100000
AMLMvec=vector(mode="numeric",length=trials)

alpha=0.7
sig=1.0

p1ft=0.145
p2ft=0.145
p13pt=0.1211
p23pt=0.1211
p12pt=0.269
p22pt=0.269
p1make=p13pt+p12pt+p1ft
p2make=p13pt+p12pt+p1ft


for(j in 1:trials){
  eps<-c(sig*rnorm(nobs))
  ran1<-(runif(nobs, min = 0, max = 1))
  ran2<-(runif(nobs, min = 0, max = 1))
  mar<-c(1:nobs)*0
  score1<-c(1:nobs)*0
  score2<-c(1:nobs)*0
  for(k in 2:nobs){
    if (mar[k-1]>0) {alpha1=alpha} & {alpha2=1
    } else {alpha1=1} & {alpha2=alpha
    }
    
    if (ran1[k]<1-alpha1*p1make) {score1[k]=0
    } else if (ran1[k]<1-alpha1*p1make+alpha1*p12pt) {score1[k]=2
    } else if (ran1[k]<1-alpha1*p1make+alpha1*p13pt) {score1[k]=3
    } else {score1[k]=1
    }
    if (ran2[k]<1-alpha2*p2make) {score2[k]=0
    } else if (ran2[k]<1-alpha2*p2make+alpha2*p22pt) {score2[k]=2
    } else if (ran2[k]<1-alpha2*p2make+alpha2*p23pt) {score2[k]=3
    } else {score2[k]=1
    }
    mar[k]=mar[k-1]+score1[k]-score2[k] 
    
  }
dmar<-c(0,diff(mar))
mar1<-mar[2:nobs]
reg<-lm(mar[1:nobs-1]~mar1)
sigsqhat<-mean(reg$residuals**2)
kap<-sum(mar[1:nobs-1]*dmar[2:nobs])/(nobs*sigsqhat)
ind<-ifelse(kap<0,1,0)
AMLM<-ind*((sum(mar[1:nobs-1]*dmar[2:nobs]))^2)/(sigsqhat*sum(mar[1:nobs-1]^2))+(sum((mar[1:nobs-1]^2)*((dmar[2:nobs]^2)-sigsqhat))^2)/(2*sigsqhat*(2*sum((mar[1:nobs-1]^4)*(dmar[2:nobs]^2))-sigsqhat*sum(mar[1:nobs-1]^4)))
#ifelse(AMLM<(-500),break,next)
AMLMvec[j]=AMLM}

hist(AMLMvec,breaks="Freedman-Diaconis",freq=FALSE,xlim=c(0,20))
quantile(AMLMvec, probs=c(0.05,0.1,0.5,0.9,0.95))

