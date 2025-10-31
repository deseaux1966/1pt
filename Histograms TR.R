#Makes multiple histogram plots

rgb(0,0.25,0.75, 0.5, names = "bluetr", maxColorValue = 1)
rgb(0,1,0, 0.5, names = "greentr", maxColorValue = 1)
rgb(1.0,1.0,0.0, 0.5, names = "yeltr", maxColorValue = 1)

h1<-hist(TRvec1,freq=FALSE,breaks="Freedman-Diaconis", xlim=c(0.0,0.5),ylim=c(0.0,6),xlab="AMLM stat",main=NULL)
br<-length(h1$breaks)
h1NBA<-hist(TRvecNBA,freq=FALSE,breaks="Freedman-Diaconis", xlim=c(0.0,0.5),ylim=c(0.0,6),xlab="AMLM stat",main=NULL)
brNBA<-length(h1NBA$breaks)

hist(TRvec07,col="#FFFF0080",freq=FALSE,breaks=br/5, xlim=c(0.0,0.5),ylim=c(0.0,7),xlab="TR stat",main=NULL)
hist(TRvec1, col="#00FF0080",freq=FALSE,breaks=br/5, add=TRUE)
hist(TRvecNBA, col="#0040BF80",freq=FALSE,breaks=2*brNBA, add=TRUE)

#H0 H1 legend:
#legend("topright", c(expression(paste("H"[0],": ",alpha," = 1.0")),expression(paste("H"[1],": ",alpha," = 0.7"))), fill=c("#FFFF0080","#00FF0080"),box.lty=0,xjust=1)

#H0 H1 NBA legend:
legend("topright", c(expression(paste("H"[0],": ",alpha," = 0.7")),expression(paste("H"[1],": ",alpha," = 1.0")),"NBA"), fill=c("#FFFF0080","#00FF0080","#0040BF80"),box.lty=0,xjust=1)

