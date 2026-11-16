#Tests two alternatives to full lineup fixed effects on all seasons.
#Team FE: offensive and defensive team by season fixed effects, which have thousands of possessions each.
#Long stint: offensive lineup by season and defensive team by season fixed effects, for lineups
#with at least minposs possessions in the season.
#Each estimate is compared with a placebo where points are shuffled within a season and the
#margin is rebuilt, so the true effect of the margin is zero.

library(data.table)

regular=1
minposs<-200
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
  #Sorting each distinct lineup once is much faster than sorting every row
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

#Two way fixed effects by alternating demeaning, with game clustered standard errors
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
  c(b=b,se=se,iter=it)
}

subs<-function(p) list("Full Sample"=p,"Full, |margin|<20"=p[abs(maradj)<20],
                       "4th Quarter"=p[period==4],"Last 5"=p[period==4 & sec_remaining<300])

poss<-rbindlist(lapply(seasons,function(s){r<-makeposs(s);cat("read",s,"\n");r}))
setorder(poss,id,period)
counts<-poss[,.N,by=offlineup]
longlines<-counts[N>=minposs,offlineup]
cat("possessions:",nrow(poss)," lineups:",nrow(counts)," lineups with at least",minposs,":",length(longlines),
    " share of possessions in them:",sum(counts[N>=minposs,N])/nrow(poss),"\n")
setmargin(poss)
poss[,points:=as.numeric(points)]
poss[,long:=offlineup %in% longlines]

runall<-function(p,draw){
  S<-subs(p)
  rbindlist(lapply(names(S),function(n){
    d<-S[[n]]; l<-d[long==TRUE]
    t<-fereg(d,"offteam","defteam"); s<-fereg(l,"offlineup","defteam"); tl<-fereg(l,"offteam","defteam")
    #Season means removed so the comparison is not driven by the trend in scoring
    d2<-copy(d); d2[,points:=points-mean(points),by=season]
    rbind(data.table(draw=draw,sample=n,estimator="Team FE",N=nrow(d),delta=t["b"],se=t["se"],iter=t["iter"]),
          data.table(draw=draw,sample=n,estimator="Long stint FE",N=nrow(l),delta=s["b"],se=s["se"],iter=s["iter"]),
          data.table(draw=draw,sample=n,estimator="Team FE, long stint sample",N=nrow(l),delta=tl["b"],se=tl["se"],iter=tl["iter"]),
          data.table(draw=draw,sample=n,estimator="No FE",N=nrow(d),delta=coef(lm(points~maradj,data=d2))[2],se=NA_real_,iter=NA_real_))
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

summ<-plc[,.(placebo=mean(delta),placebosd=sd(delta)),by=.(sample,estimator)]
res<-merge(actual[,.(sample,estimator,N,delta,se,iter)],summ,by=c("sample","estimator"))
res[,net:=delta-placebo]
res[,tcl:=delta/se]
setorder(res,estimator,sample)
options(width=170,digits=5)
print(res)
fwrite(res,"margin FE checks all seasons.csv")
