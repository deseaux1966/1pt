#Generates the possession level tables for the paper from the corrected possession data.
#Pooled estimates for all seasons with and without team by season fixed effects, for points
#per possession and for a turnover dummy, and estimates by season. Standard errors are clustered by game.

library(data.table)

regular=1
datadir<-"C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons"
seasons<-paste0(2002:2021,"-",2003:2022)

makeposs<-function(season){
  fn<-file.path(datadir,season,"combined-stats.csv")
  keep<-c("period","away_score","remaining_time","team","event_type","points")
  dfNBA<-fread(fn,na.strings=c("","NA"),fill=Inf,select=c(1,which(names(fread(fn,nrows=0)) %in% keep)))
  setnames(dfNBA,1,"id")
  dfNBA[,id:=as.numeric(gsub("[^0-9]","",id))]
  dfNBA<-dfNBA[!is.na(id)]
  if(regular == 1) dfNBA<-dfNBA[id<30000000] else dfNBA<-dfNBA[id>30000000]
  tm<-tstrsplit(dfNBA$remaining_time,":",type.convert=TRUE)
  dfNBA[,sec_remaining:=tm[[1]]*3600+tm[[2]]*60+tm[[3]]]
  dfNBA<-dfNBA[event_type %in% c("shot","free throw","turnover") & !is.na(team)]
  dfNBA[is.na(points),points:=0]
  dfNBA[,away_team:=team[which(away_score>0)[1]],by=id]
  dfNBA[,home_team:=setdiff(unique(team),away_team)[1],by=id]
  dfNBA[,sgn:=ifelse(team==away_team,-1,1)]
  dfNBA[,run:=rleid(team),by=.(id,period)]
  p<-dfNBA[,.(points=as.numeric(sum(points)),turnover=as.numeric(any(event_type=="turnover")),
              team=team[1],opp=ifelse(sgn[1]==1,away_team[1],home_team[1]),
              sgn=sgn[1],sec_remaining=sec_remaining[1]),by=.(id,period,run)]
  setorder(p,id,period,run)
  p[,marhome:=shift(cumsum(sgn*points),fill=0),by=id]
  p[,maradj:=sgn*marhome]
  p[,offteam:=paste(season,team)]
  p[,defteam:=paste(season,opp)]
  p[,season:=season]
  p[]
}

fereg<-function(d,yvar,f1,f2,tol=1e-6,maxit=5000){
  y<-as.numeric(d[[yvar]]); x<-as.numeric(d$maradj)
  o<-as.integer(factor(d[[f1]])); g<-as.integer(factor(d[[f2]]))
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
  score<-rowsum(x*e,d$id)
  m<-nrow(score)
  se<-sqrt(m/(m-1)*sum(score^2))/sum(x^2)
  c(b=b,se=se,p=2*pnorm(-abs(b/se)))
}

subs<-function(p) list("Full Sample"=p,"Full Sample, $|$margin$|<$20"=p[abs(maradj)<20],
  "Full Sample, $|$margin$|<$10"=p[abs(maradj)<10],"Full Sample, $|$margin$|<$5"=p[abs(maradj)<5],
  "4th Quarter"=p[period==4],"4th Quarter, $|$margin$|<$20"=p[period==4 & abs(maradj)<20],
  "4th Quarter, $|$margin$|<$10"=p[period==4 & abs(maradj)<10],"4th Quarter, $|$margin$|<$5"=p[period==4 & abs(maradj)<5],
  "Last 5 Minutes"=p[period==4 & sec_remaining<300],"Last 5 Minutes, $|$margin$|<$20"=p[period==4 & sec_remaining<300 & abs(maradj)<20],
  "Last 5 Minutes, $|$margin$|<$10"=p[period==4 & sec_remaining<300 & abs(maradj)<10],"Last 5 Minutes, $|$margin$|<$5"=p[period==4 & sec_remaining<300 & abs(maradj)<5])

poss<-rbindlist(lapply(seasons,makeposs))
cat("possessions:",nrow(poss)," games:",uniqueN(poss$id)," points per possession:",mean(poss$points)," turnovers per possession:",mean(poss$turnover),"\n")

pooled<-function(yvar){
  S<-subs(poss)
  rbindlist(lapply(names(S),function(n){
    d<-S[[n]]
    a<-fereg(d,yvar,"season","season"); t<-fereg(d,yvar,"offteam","defteam")
    data.table(sample=n,N=nrow(d),mean=mean(d[[yvar]]),delta=a["b"],se=a["se"],p=a["p"],deltaFE=t["b"],seFE=t["se"],pFE=t["p"])
  }))
}
scores<-pooled("points")
turnovers<-pooled("turnover")

byseason<-rbindlist(lapply(seasons,function(s){
  p<-poss[season==s]
  S<-list(Full=p,Q4=p[period==4],L5=p[period==4 & sec_remaining<300])
  o<-data.table(season=s,games=uniqueN(p$id))
  for(n in names(S)){
    t<-fereg(S[[n]],"points","offteam","defteam")
    o[,paste0(n,c("b","se")):=.(t["b"],t["se"])]
  }
  o
}))

options(width=170,digits=5)
print(scores); print(turnovers); print(byseason)
fwrite(scores,"table scores corrected.csv")
fwrite(turnovers,"table turnovers corrected.csv")
fwrite(byseason,"table by season corrected.csv")
