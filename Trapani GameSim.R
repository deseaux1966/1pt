library(tseries)
nobs=125
prenobs=0
trials=100000
TRvec=vector(mode="numeric",length=trials)
H=ceiling(nobs^0.25)

alpha=0.5
sig = 1.0
a=0.5

p1ft=0.17
p2ft=0.17
p13pt=0.127
p23pt=0.127
p12pt=0.413
p22pt=0.413
p1miss=1-p13pt-p12pt-p1ft
p2miss=1-p13pt-p12pt-p1ft


for(j in 1:trials){
  eps<-c(sig*rnorm(prenobs+nobs))
  ran1<-(runif((prenobs+nobs), min = 0, max = 1))
  ran2<-(runif((prenobs+nobs), min = 0, max = 1))
  mar<-c(1:(prenobs+nobs))*0
  score1<-c(1:(prenobs+nobs))*0
  score2<-c(1:(prenobs+nobs))*0
  for(k in 2:(prenobs+nobs)){if (mar[k-1]>0) {alpha1=alpha} & {alpha2=1
  } else {alpha1=1} & {alpha2=alpha
  }
    
    if (ran1[k]<alpha1*p1miss) {score1[k]=0
    } else if (ran1[k]<alpha1*p1miss+alpha1*p12pt) {score1[k]=2
    } else if (ran1[k]<alpha1*p1miss+alpha1*p13pt) {score1[k]=3
    } else {score1[k]=1
    }
    if (ran2[k]<alpha2*p1miss) {score2[k]=0
    } else if (ran2[k]<alpha2*p1miss+alpha2*p12pt) {score2[k]=2
    } else if (ran2[k]<alpha2*p1miss+alpha2*p13pt) {score2[k]=3
    } else {score2[k]=1
    }
    mar[k]=mar[k-1]+score1[k]-score2[k] }
  
  Y<-a/(a+mar^2)
  rjlist<-acf(Y[(prenobs+1):(prenobs+nobs)],lag.max=10,type="covariance",plot="False",demean="True")
  rj<-rjlist$acf
  w=c(1-(1:H)/(H+1))
  TRvec[j]<-rj[1]+2*sum(rj[2:(H+1)]*w[1:H])
  }

hist(TRvec,breaks=50,xlim=c(0,0.5))
