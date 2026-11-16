#Margin regressions for all seasons, using the corrected possession data from
#"shooting % v margin ranges FE corrected.R". There are no lineup fixed effects, since
#those estimates are biased (see the placebo test). Writes results for each season and pooled.

library(data.table)

regular=1
datadir<-"C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons"
seasons<-paste0(2002:2021,"-",2003:2022)

makeposs<-function(season){
  dfNBA<-fread(file.path(datadir,season,"combined-stats.csv"),na.strings=c("","NA"),fill=Inf,
               select=c(1,which(names(fread(file.path(datadir,season,"combined-stats.csv"),nrows=0)) %in%
                 c("period","away_score","home_score","remaining_time","team","event_type","points"))))
  setnames(dfNBA,1,"id")
  dfNBA[,id:=as.numeric(gsub("[^0-9]","",id))]
  dfNBA<-dfNBA[!is.na(id)]
  if(regular == 1) {
    dfNBA<-dfNBA[id<30000000]
  } else {
    dfNBA<-dfNBA[id>30000000]
  }
  tm<-tstrsplit(dfNBA$remaining_time,":",type.convert=TRUE)
  dfNBA[,sec_remaining:=tm[[1]]*3600+tm[[2]]*60+tm[[3]]]
  dfNBA<-dfNBA[event_type %in% c("shot","free throw","turnover") & !is.na(team)]
  dfNBA[is.na(points),points:=0]
  dfNBA[,away_team:=team[which(away_score>0)[1]],by=id]
  dfNBA[,sgn:=ifelse(team==away_team,-1,1)]
  dfNBA[,run:=rleid(team),by=.(id,period)]
  dfposs<-dfNBA[,.(points=sum(points),sgn=sgn[1],sec_remaining=sec_remaining[1]),by=.(id,period,run)]
  setorder(dfposs,id,period,run)
  dfposs[,marhome:=shift(cumsum(sgn*points),fill=0),by=id]
  dfposs[,maradj:=sgn*marhome]
  dfposs[,season:=season]
  final<-dfposs[,.(calc=sum(points*(sgn==1)),calcaway=sum(points*(sgn==-1))),by=id]
  actual<-dfNBA[,.(h=max(home_score,na.rm=TRUE),a=max(away_score,na.rm=TRUE)),by=id]
  chk<-merge(final,actual,by="id")
  checks<-data.table(season=season,games=uniqueN(dfposs$id),possperteam=nrow(dfposs)/(2*uniqueN(dfposs$id)),
                     ptsper100=100*mean(dfposs$points),scorematch=chk[calc==h & calcaway==a,.N])
  list(poss=dfposs,checks=checks)
}

estimate<-function(p,label){
  S<-list("Full Sample"=p,"Full, |margin|<20"=p[abs(maradj)<20],"Full, |margin|<10"=p[abs(maradj)<10],
          "Full, |margin|<5"=p[abs(maradj)<5],"4th Quarter"=p[period==4],
          "4th, |margin|<20"=p[period==4 & abs(maradj)<20],"4th, |margin|<10"=p[period==4 & abs(maradj)<10],
          "4th, |margin|<5"=p[period==4 & abs(maradj)<5],"Last 5"=p[period==4 & sec_remaining<300],
          "Last 5, |margin|<20"=p[period==4 & sec_remaining<300 & abs(maradj)<20],
          "Last 5, |margin|<10"=p[period==4 & sec_remaining<300 & abs(maradj)<10],
          "Last 5, |margin|<5"=p[period==4 & sec_remaining<300 & abs(maradj)<5])
  rbindlist(lapply(names(S),function(n){
    d<-S[[n]]
    co<-summary(lm(points~maradj,data=d))$coefficients
    data.table(season=label,sample=n,N=nrow(d),meanscore=mean(d$points),delta=co[2,1],se=co[2,2],p=co[2,4])
  }))
}

allposs<-list(); allchecks<-list(); allres<-list()
for(s in seasons){
  r<-tryCatch(makeposs(s),error=function(e){cat("FAILED",s,conditionMessage(e),"\n");NULL})
  if(is.null(r)) next
  allposs[[s]]<-r$poss; allchecks[[s]]<-r$checks
  allres[[s]]<-estimate(r$poss,s)
  cat("done",s,"\n")
}
pooled<-rbindlist(allposs)
#Season means removed, so the pooled estimates do not reflect the trend in scoring over time
pooled[,points:=as.numeric(points)]
grandmean<-mean(pooled$points)
pooled[,points:=points-mean(points)+grandmean,by=season]
res<-rbind(rbindlist(allres),estimate(pooled,"pooled"))

options(width=170,digits=5)
print(rbindlist(allchecks))
wide<-dcast(res[sample %in% c("Full Sample","Full, |margin|<20","4th Quarter","4th, |margin|<20","Last 5","Last 5, |margin|<20")],season~sample,value.var="delta")
print(wide)
print(res[season=="pooled"])
sig<-res[season!="pooled",.(negative=sum(delta<0),negsig05=sum(delta<0 & p<0.05),possig05=sum(delta>0 & p<0.05),mean=mean(delta)),by=sample]
print(sig)
fwrite(res,"margin results all seasons corrected.csv")
fwrite(rbindlist(allchecks),"possession checks all seasons.csv")
