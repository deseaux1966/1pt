#Margin plot and AMLM statistics for one game, consistent with "AMLM tables corrected.R":
#the margin is observed at each shot or free throw of the team in the first row (the opening jump ball),
#the trend uses the closing spread divided by possessions, and the estimated trend uses the corrected
#procedure. p-values come from simulations under the null with the season's shooting probabilities
#(Tables S6 and S7), with the same detrending for the estimated trend.

library(readxl)
library(ggplot2)

base_path<-"C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons/"
outdir<-"C:/Users/gawater/Documents/GitHub/Latex files/1pt/"

amlm<-function(m1,dm,s2){
  k<-rowSums(m1*dm)
  (k<0)*k^2/(s2*rowSums(m1^2))+rowSums(m1^2*(dm^2-s2))^2/(2*s2*(2*rowSums(m1^4*dm^2)-s2*rowSums(m1^4)))
}
esttrend<-function(M){
  n<-ncol(M); t<-1:n
  beta1<-(M[,n]-M[,1])/(n-1); md<-M-outer(beta1,t)
  rho<-rowSums(md[,1:(n-1),drop=FALSE]*md[,2:n,drop=FALSE])/rowSums(md[,1:(n-1),drop=FALSE]^2)
  y<-M[,2:n,drop=FALSE]-rho*M[,1:(n-1),drop=FALSE]; x<-outer(1-rho,2:n)+rho
  betahat<-rowSums(x*y)/rowSums(x^2); mar<-M-outer(betahat,t)
  m1<-mar[,1:(n-1),drop=FALSE]; dm<-mar[,2:n,drop=FALSE]-m1
  list(AMLM=amlm(m1,dm,rowMeans(dm^2)),rho=rowSums(m1*mar[,2:n,drop=FALSE])/rowSums(m1^2),betahat=betahat)
}
#statistics as in AMLM test NBA data.R (no trend) and the spread program (spread trend b per observation)
notrend<-function(m){
  n<-length(m); x<-m[2:n]; reg<-lm(m[1:(n-1)]~0+x)
  list(AMLM=amlm(rbind(m[1:(n-1)]),rbind(diff(m)),sigma(reg)^2),rho=unname(coef(reg)))
}
withspread<-function(m,b){
  n<-length(m); md<-m-b*(1:n); x<-md[2:n]; reg<-lm(md[1:(n-1)]~0+x)
  dm<-c(0,diff(md)); list(AMLM=amlm(rbind(md[1:(n-1)]),rbind(dm[2:n]),mean(dm[1:(n-1)]^2)),rho=unname(coef(reg)))
}
nullsims<-function(p,trials=100000,n=101,seed=1){
  set.seed(seed)
  draw<-function(){u<-runif(trials); pm<-sum(p); ifelse(u<1-pm,0,ifelse(u<1-pm+p[2],2,ifelse(u<1-pm+p[2]+p[3],3,1)))}
  M<-matrix(0,trials,n); for(k in 2:n) M[,k]<-M[,k-1]+draw()-draw()
  y<-M[,1:(n-1)]; x<-M[,2:n]; dm<-x-y; yc<-y-rowMeans(y); xc<-x-rowMeans(x)
  s2<-(rowSums(yc^2)-rowSums(xc*yc)^2/rowSums(xc^2))/(n-3)
  list(null=amlm(y,dm,s2),tr=esttrend(M)$AMLM)
}

gamestats<-function(season,gameid,p,title,file,width=6,height=3.9,showest=FALSE){
  d<-read.delim(paste0(base_path,season,"/combined-stats.csv"),header=TRUE,sep=",",stringsAsFactors=FALSE)
  names(d)[1]<-"id"; d<-d[-1,]; d[d==""]<-NA
  d$id<-suppressWarnings(as.numeric(d$id)); d<-d[!is.na(d$id) & d$id==as.numeric(gameid),]
  d<-subset(d,event_type=="shot"|type=="free throw 2/2"|type=="free throw 1/1"|(event_type=="jump ball" & play_id==2))
  d$team[is.na(d$team)]<-"0"
  d<-d[d$team==d$team[1],]
  m<-d$home_score-d$away_score; m[is.na(m)]<-0
  ts<-data.frame(read_excel(paste0(base_path,season,"/Team-Stats.xlsx")))
  h<-ts[as.numeric(ts$GAME.ID)==as.numeric(gameid) & ts$VENUE=="H",]
  b<-(-1)*as.numeric(h$CLOSING.SPREAD)/as.numeric(h$POSS)
  sims<-nullsims(p)
  a<-notrend(m); s<-withspread(m,b); e<-esttrend(rbind(m))
  out<-data.frame(stat=c("rho","AMLM","p"),
    margin=c(a$rho,a$AMLM,mean(sims$null>a$AMLM)),
    spread=c(s$rho,s$AMLM,mean(sims$null>s$AMLM)),
    est=c(e$rho,e$AMLM,mean(sims$tr>e$AMLM)))
  n<-length(m); pd<-data.frame(poss=1:n,mar=m)
  g<-ggplot(pd,aes(x=poss,y=mar))+geom_line(color="blue",lwd=0.5)+
    geom_abline(slope=b,intercept=0,color="magenta",linetype="dashed",lwd=0.4)+
    annotate("point",x=n,y=b*n,color="magenta",size=3)+
    annotate("text",x=n,y=b*n,label="spread",hjust=1.1,vjust=-0.6,color="magenta")+
    scale_x_continuous(limits=c(0,n),expand=expansion(mult=c(0,0.01)))+
    labs(x="Possessions",y="Margin",title=title)+theme_minimal()+theme(plot.title=element_text(size=10,hjust=0.5))
  if(showest) g<-g+geom_abline(slope=e$betahat,intercept=0,color="darkgreen",linetype="dashed",lwd=0.4)+
    annotate("text",x=n,y=e$betahat*n,label="estimated trend",hjust=1.1,vjust=3.5,color="darkgreen")
  ggsave(paste0(outdir,file),g,width=width,height=height,dpi=200,bg="white")
  cat("\n",title," closing spread (home):",h$CLOSING.SPREAD," possessions:",h$POSS," observations:",n,"\n")
  print(format(out,digits=3))
  invisible(out)
}

#2013 NBA Finals game 7, San Antonio at Miami (Figure 1); 2012-13 playoff probabilities from Table S7
gamestats("2012-2013","0041200407",c(0.083,0.355,0.084),"SAS@MIA [2013-06-20]","SAS@MIA 6-20-13.png")
#2016 NBA Finals game 7, Cleveland at Golden State (Figure 3c); 2015-16 playoff probabilities from Table S7
gamestats("2015-2016","0041500407",c(0.075,0.340,0.105),"CLE@GSW [2016-06-19]","CLE@GSW 6-19-16.png",width=5.6,height=2.8)
#Figure 3a: Golden State at Brooklyn, 2020-21 regular season probabilities from Table S6
gamestats("2020-2021","0022000001",c(0.074,0.331,0.133),"GSW@BKN [2020-12-22]","GSW@BKN 12-22-20.png",width=5.6,height=2.8)
#Figure 3b: Houston at LA Clippers, 2014-15 playoff probabilities from Table S7
gamestats("2014-2015","0041400236",c(0.079,0.345,0.099),"HOU@LAC [2015-05-14]","HOU@LAC 5-14-15.png",width=5.6,height=2.8)
#Figure 3d: Milwaukee at Atlanta, 2020-21 playoff probabilities from Table S7, with the estimated trend
gamestats("2020-2021","0042000304",c(0.068,0.334,0.131),"MIL@ATL [2021-06-29]","MIL@ATL 6-29-21 corrected.png",width=5.6,height=2.8,showest=TRUE)
