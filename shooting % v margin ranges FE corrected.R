#Corrected version of "shooting % v margin ranges FE.R".
#1. Quarter and end of game samples use the period as well as the clock.
#2. The Last 5 Minutes sample has no margin restriction.
#3. Lineups are true two way fixed effects, not integer codes.
#4. A possession sums all points in a team's run of events and includes turnovers.

library(data.table)

season<-"2021-2022"
regular=1
datadir<-"C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons"

dfNBA<-fread(file.path(datadir,season,"combined-stats.csv"),na.strings=c("","NA"))
setnames(dfNBA,1,"id")
dfNBA<-dfNBA[!is.na(id)]
dfNBA[,id:=as.numeric(id)]
if(regular == 1) {
  dfNBA<-dfNBA[id<30000000]
} else {
  dfNBA<-dfNBA[id>30000000]
}

#remaining_time counts down within each period
tm<-tstrsplit(dfNBA$remaining_time,":",type.convert=TRUE)
dfNBA[,sec_remaining:=tm[[1]]*3600+tm[[2]]*60+tm[[3]]]

#Shots and free throws score points. Turnovers end possessions with no shot.
dfNBA<-dfNBA[event_type %in% c("shot","free throw","turnover")]
dfNBA[is.na(points),points:=0]
dfNBA[,away_team:=team[which(away_score>0)[1]],by=id]
dfNBA[,sgn:=ifelse(team==away_team,-1,1)]

#A possession is a run of events by one team within a period
dfNBA[,run:=rleid(team),by=.(id,period)]
dfposs<-dfNBA[,.(points=sum(points),team=team[1],sgn=sgn[1],sec_remaining=sec_remaining[1],
                 h1=h1[1],h2=h2[1],h3=h3[1],h4=h4[1],h5=h5[1],
                 a1=a1[1],a2=a2[1],a3=a3[1],a4=a4[1],a5=a5[1]),
              by=.(id,period,run)]
setorder(dfposs,id,period,run)

#Margin before the possession, from the perspective of the team with the ball
dfposs[,marhome:=shift(cumsum(sgn*points),fill=0),by=id]
dfposs[,maradj:=sgn*marhome]

#Sorting the names keeps a lineup the same regardless of the order listed
lineup<-function(m) apply(m,1,function(x) paste(sort(x),collapse="|"))
homeline<-lineup(as.matrix(dfposs[,.(h1,h2,h3,h4,h5)]))
awayline<-lineup(as.matrix(dfposs[,.(a1,a2,a3,a4,a5)]))
dfposs[,offlineup:=ifelse(sgn==1,homeline,awayline)]
dfposs[,deflineup:=ifelse(sgn==1,awayline,homeline)]

#Two way fixed effects by alternating demeaning. Reports iid and game clustered p-values.
fereg<-function(d,tol=1e-6,maxit=5000){
  y<-as.numeric(d$points); x<-as.numeric(d$maradj)
  o<-as.integer(factor(d$offlineup)); g<-as.integer(factor(d$deflineup))
  no<-tabulate(o); ng0<-tabulate(g)
  demean<-function(v,k,n) v-(rowsum(v,k)[,1]/n)[k]
  for(it in 1:maxit){
    y0<-y; x0<-x
    y<-demean(y,o,no); y<-demean(y,g,ng0)
    x<-demean(x,o,no); x<-demean(x,g,ng0)
    if(max(abs(y-y0),abs(x-x0))<tol) break
  }
  b<-sum(x*y)/sum(x^2)
  e<-y-b*x
  dfree<-length(y)-max(o)-max(g)
  se<-sqrt(sum(e^2)/dfree/sum(x^2))
  score<-rowsum(x*e,d$id)
  ng<-nrow(score)
  secl<-sqrt(ng/(ng-1)*sum(score^2))/sum(x^2)
  c(b=b,p=2*pt(-abs(b/se),dfree),pcl=2*pnorm(-abs(b/secl)),iter=it)
}

samples<-list(
  "Full Sample"=dfposs,
  "Full Sample, |margin|<20"=dfposs[abs(maradj)<20],
  "Full Sample, |margin|<10"=dfposs[abs(maradj)<10],
  "Full Sample, |margin|<5"=dfposs[abs(maradj)<5],
  "4th Quarter"=dfposs[period==4],
  "4th Quarter, |margin|<20"=dfposs[period==4 & abs(maradj)<20],
  "4th Quarter, |margin|<10"=dfposs[period==4 & abs(maradj)<10],
  "4th Quarter, |margin|<5"=dfposs[period==4 & abs(maradj)<5],
  "Last 5 Minutes"=dfposs[period==4 & sec_remaining<300],
  "Last 5 minutes, |margin|<20"=dfposs[period==4 & sec_remaining<300 & abs(maradj)<20],
  "Last 5 minutes, |margin|<10"=dfposs[period==4 & sec_remaining<300 & abs(maradj)<10],
  "Last 5 minutes, |margin|<5"=dfposs[period==4 & sec_remaining<300 & abs(maradj)<5])

results<-rbindlist(lapply(names(samples),function(s){
  d<-samples[[s]]
  reg<-summary(lm(points~maradj,data=d))$coefficients
  fe<-fereg(d)
  data.table(sample=s,N=nrow(d),meanscore=mean(d$points),semean=sd(d$points)/sqrt(nrow(d)),
             margin=reg[2,1],p=reg[2,4],marginFE=fe["b"],pFE=fe["p"],pFEcluster=fe["pcl"],iter=fe["iter"])
}))

#Checks on the possession data
games<-uniqueN(dfposs$id)
final<-dfposs[,.(calc=sum(points*(sgn==1)),calcaway=sum(points*(sgn==-1))),by=id]
actual<-dfNBA[,.(h=max(home_score,na.rm=TRUE),a=max(away_score,na.rm=TRUE)),by=id]
chk<-merge(final,actual,by="id")
cat("games:",games," possessions:",nrow(dfposs),"\n")
cat("possessions per team per game:",nrow(dfposs)/(2*games),"\n")
cat("points per team per game:",sum(dfposs$points)/(2*games),"\n")
cat("points per 100 possessions:",100*mean(dfposs$points),"\n")
cat("games where summed points match the final score:",chk[calc==h & calcaway==a,.N],"\n")
cat("offensive lineups:",uniqueN(dfposs$offlineup)," defensive lineups:",uniqueN(dfposs$deflineup),"\n\n")

options(width=160,digits=5)
print(results)
fwrite(results,paste0("margin results ",season," corrected.csv"))
