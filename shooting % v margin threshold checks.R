#Sensitivity of the high use lineup results to the threshold for high use.
#For each threshold, lineups with at least that many possessions in a season are kept, and equation
#(poss reg) is estimated with offensive lineup by season and defensive team by season fixed effects,
#and with team by season fixed effects on the same possessions. Each estimate is compared with a placebo
#where points are shuffled within a season and the margin is rebuilt, so the true effect is zero.
#Uses makeposs, setmargin and fereg from "shooting % v margin FE checks.R".

library(data.table)

regular=1
thresholds<-c(50,100,200,400,800)
draws<-5
datadir<-"C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons"
seasons<-paste0(2002:2021,"-",2003:2022)
set.seed(1)

makeposs<-function(season){
  fn<-file.path(datadir,season,"combined-stats.csv")
  keep<-c("period","away_score","remaining_time","team","event_type","points",
          "a1","a2","a3","a4","a5","h1","h2","h3","h4","h5")
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
  p<-dfNBA[,.(points=sum(points),team=team[1],opp=ifelse(sgn[1]==1,away_team[1],home_team[1]),
              sgn=sgn[1],sec_remaining=sec_remaining[1],
              h1=h1[1],h2=h2[1],h3=h3[1],h4=h4[1],h5=h5[1],
              a1=a1[1],a2=a2[1],a3=a3[1],a4=a4[1],a5=a5[1]),by=.(id,period,run)]
  setorder(p,id,period,run)
  sortline<-function(d,cols){
    u<-unique(d[,..cols]); u[,lab:=apply(as.matrix(.SD),1,function(x) paste(sort(x),collapse="|")),.SDcols=cols]
    u[d,on=cols]$lab
  }
  hl<-sortline(p,paste0("h",1:5)); al<-sortline(p,paste0("a",1:5))
  p[,offlineup:=paste(season,team,ifelse(sgn==1,hl,al))]
  p[,offteam:=paste(season,team)]
  p[,defteam:=paste(season,opp)]
  p[,season:=season]
  p[,c(paste0("h",1:5),paste0("a",1:5),"run","team","opp"):=NULL]
  p[]
}

setmargin<-function(p){
  p[,marhome:=shift(cumsum(sgn*points),fill=0),by=id]
  p[,maradj:=sgn*marhome]
  p[]
}

fereg<-function(d,f1,f2,tol=1e-6,maxit=5000){
  y<-as.numeric(d$points); x<-as.numeric(d$maradj)
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
  c(b=b,se=se)
}

poss<-rbindlist(lapply(seasons,function(s){r<-makeposs(s);cat("read",s,"\n");r}))
setorder(poss,id,period)
counts<-poss[,.N,by=offlineup]
poss[counts,nline:=i.N,on="offlineup"]
setmargin(poss)
poss[,points:=as.numeric(points)]
info<-rbindlist(lapply(thresholds,function(k) data.table(threshold=k,lineups=sum(counts$N>=k),
  share=sum(counts[N>=k,N])/nrow(poss),posslineup=mean(counts[N>=k,N]))))
print(info)

subs<-function(p) list("Full Sample"=p,"4th Quarter"=p[period==4],"Last 5"=p[period==4 & sec_remaining<300])

runall<-function(p,draw){
  rbindlist(lapply(thresholds,function(k){
    S<-subs(p[nline>=k])
    rbindlist(lapply(names(S),function(n){
      l<-S[[n]]
      s<-fereg(l,"offlineup","defteam"); t<-fereg(l,"offteam","defteam")
      rbind(data.table(draw=draw,threshold=k,sample=n,estimator="Lineup FE",N=nrow(l),delta=s["b"],se=s["se"]),
            data.table(draw=draw,threshold=k,sample=n,estimator="Team FE",N=nrow(l),delta=t["b"],se=t["se"]))
    }))
  }))
}

actual<-runall(poss,0)
cat("actual done\n")
plc<-rbindlist(lapply(1:draws,function(r){
  p<-copy(poss)
  p[,points:=sample(points),by=season]
  setmargin(p)
  out<-runall(p,r); cat("placebo",r,"done\n"); out
}))

summ<-plc[,.(placebo=mean(delta),placebosd=sd(delta)),by=.(threshold,sample,estimator)]
res<-merge(actual[,.(threshold,sample,estimator,N,delta,se)],summ,by=c("threshold","sample","estimator"))
res[,net:=delta-placebo]
setorder(res,sample,estimator,threshold)
options(width=170,digits=5)
print(res)
fwrite(res,"margin threshold checks all seasons.csv")
fwrite(info,"margin threshold lineups.csv")
