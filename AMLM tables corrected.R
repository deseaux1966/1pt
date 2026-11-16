#Corrected version of "combined programs.R" and the programs it sources, for Tables S8 and S9.
#Corrections:
# 1. Spread trend: slope is the spread divided by possessions, -spread/POSS, not -spread/(2*POSS).
#    The trend uses the closing spread (spreadvar), set just before the game.
# 2. Spreads are matched to games by game id, not by row position (2017-18 has one game missing
#    from the play by play data, which shifted every later spread by one game).
# 3. Estimated trend: preliminary slope from the mean difference, forward regression for rho, and
#    quasi-differenced regression m(t)-rho*m(t-1) on t-rho*(t-1) for the slope. Critical values
#    are simulated under the null with the same detrending, since estimating the trend changes
#    the distribution of the AMLM statistic.
#The uncorrected statistics are also computed to check that they reproduce the published tables.
#Shooting probabilities and the critical values without trend are those published in Tables S6 and S7.

library(data.table)
library(readxl)

base_path<-"C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons/"
outdir<-"C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/R programs '25/"
years<-c("2021-2022","2020-2021","2019-2020","2018-2019","2017-2018","2016-2017","2015-2016",
         "2014-2015","2013-2014","2012-2013","2011-2012","2010-2011","2009-2010",
         "2008-2009","2007-2008","2006-2007","2005-2006","2004-2005","2003-2004","2002-2003")
spreadyears<-years[1:15]
spreadvar<-"CLOSING.SPREAD"   #"OPENING.SPREAD" for the opening spread; the uncorrected statistics always use the opening spread
nobssim<-101
trials<-100000
set.seed(20261104)

#Table S6 (regular season) and Table S7 (playoffs): p1, p2, p3, cr(0.1), cr(0.05), cr(0.01)
tabS6<-matrix(c(
0.075,0.329,0.132,3.398,4.788,8.828, 0.074,0.331,0.133,3.408,4.812,9.042, 0.076,0.332,0.128,3.377,4.741,8.723,
0.076,0.340,0.120,3.411,4.818,8.986, 0.075,0.344,0.115,3.398,4.831,9.069, 0.076,0.351,0.107,3.387,4.781,8.823,
0.077,0.358,0.095,3.437,4.862,9.066, 0.076,0.362,0.090,3.407,4.766,8.770, 0.079,0.365,0.089,3.396,4.808,8.832,
0.076,0.369,0.084,3.406,4.861,9.063, 0.076,0.375,0.077,3.402,4.826,8.955, 0.082,0.381,0.077,3.409,4.798,8.879,
0.081,0.383,0.076,3.425,4.806,8.801, 0.081,0.379,0.080,3.395,4.807,8.875, 0.081,0.380,0.077,3.398,4.812,8.713,
0.086,0.383,0.074,3.381,4.768,8.941, 0.088,0.382,0.070,3.437,4.799,8.812, 0.084,0.384,0.068,3.374,4.763,8.790,
0.081,0.380,0.064,3.415,4.813,8.884, 0.080,0.384,0.064,3.411,4.839,8.978),ncol=6,byrow=TRUE)
tabS7<-matrix(c(
0.077,0.319,0.132,3.424,4.865,9.264, 0.068,0.334,0.131,3.405,4.825,8.964, 0.078,0.320,0.143,3.408,4.806,8.866,
0.073,0.331,0.122,3.437,4.847,8.991, 0.077,0.343,0.114,3.401,4.770,8.877, 0.073,0.349,0.118,3.414,4.818,8.806,
0.075,0.340,0.105,3.411,4.805,9.013, 0.079,0.345,0.099,3.389,4.761,8.672, 0.078,0.363,0.094,3.410,4.809,8.932,
0.083,0.355,0.084,3.388,4.742,8.829, 0.075,0.382,0.073,3.416,4.827,8.979, 0.081,0.376,0.074,3.391,4.817,8.923,
0.088,0.379,0.079,3.378,4.755,8.704, 0.085,0.370,0.083,3.430,4.837,9.068, 0.082,0.375,0.076,3.372,4.725,8.643,
0.086,0.372,0.076,3.375,4.795,8.827, 0.087,0.391,0.072,3.390,4.829,8.981, 0.089,0.376,0.075,3.414,4.812,8.901,
0.083,0.365,0.064,3.395,4.777,8.957, 0.083,0.374,0.073,3.405,4.770,8.819),ncol=6,byrow=TRUE)

#AMLM statistic for rows of m1 = m(t-1) and dm = m(t)-m(t-1), with variance s2 (one per row)
amlm<-function(m1,dm,s2){
  k<-rowSums(m1*dm)
  (k<0)*k^2/(s2*rowSums(m1^2))+
    rowSums(m1^2*(dm^2-s2))^2/(2*s2*(2*rowSums(m1^4*dm^2)-s2*rowSums(m1^4)))
}

#Corrected estimated trend: each row of M is a margin series
esttrend<-function(M){
  n<-ncol(M); t<-1:n
  beta1<-(M[,n]-M[,1])/(n-1)
  md<-M-outer(beta1,t)
  rho<-rowSums(md[,1:(n-1),drop=FALSE]*md[,2:n,drop=FALSE])/rowSums(md[,1:(n-1),drop=FALSE]^2)
  y<-M[,2:n,drop=FALSE]-rho*M[,1:(n-1),drop=FALSE]
  x<-outer(1-rho,2:n)+rho
  betahat<-rowSums(x*y)/rowSums(x^2)
  mar<-M-outer(betahat,t)
  m1<-mar[,1:(n-1),drop=FALSE]; dm<-mar[,2:n,drop=FALSE]-m1
  s2<-rowMeans(dm^2)
  data.frame(AMLM=amlm(m1,dm,s2),rho=rowSums(m1*mar[,2:n,drop=FALSE])/rowSums(m1^2),betahat=betahat)
}

#Simulated margins under the null alpha=1, as in GameSimAMLM.R
simnull<-function(p){
  draw<-function(){u<-runif(trials); pm<-sum(p)
    ifelse(u<1-pm,0,ifelse(u<1-pm+p[2],2,ifelse(u<1-pm+p[2]+p[3],3,1)))}
  M<-matrix(0,trials,nobssim)
  for(k in 2:nobssim) M[,k]<-M[,k-1]+draw()-draw()
  M
}

#Statistics for one game as in the original programs
oldstats<-function(mar,bsp_old,bsp_new){
  nobs<-length(mar); t<-1:nobs
  out<-list()
  #AMLM test NBA data.R
  m1<-mar[2:nobs]; reg<-lm(mar[1:(nobs-1)]~0+m1)
  rho0<-unname(coef(reg)[1]); s2<-sigma(reg)^2
  dmar<-c(0,diff(mar))
  out$AMLM<-amlm(rbind(mar[1:(nobs-1)]),rbind(dmar[2:nobs]),s2); out$rho<-rho0
  #AMLMdetvar test NBA data spread.R, with the old and the corrected spread slope
  for(v in c("old","new")){
    b<-if(v=="old") bsp_old else bsp_new
    if(is.na(b)){out[[paste0("AMLMsp",v)]]<-NA; out[[paste0("rhosp",v)]]<-NA; next}
    m<-mar-b*t; m1<-m[2:nobs]; reg<-lm(m[1:(nobs-1)]~0+m1)
    dm<-c(0,diff(m)); s2<-mean(dm[1:(nobs-1)]^2)
    out[[paste0("AMLMsp",v)]]<-amlm(rbind(m[1:(nobs-1)]),rbind(dm[2:nobs]),s2)
    out[[paste0("rhosp",v)]]<-unname(coef(reg)[1])
  }
  #AMLMdetvar test NBA data est.R as coded
  dmarg<-c(0,diff(mar)); beta<-mean(dmarg); md<-mar-beta*t
  md1<-md[2:nobs]; rhohat<-unname(coef(lm(md[1:(nobs-1)]~0+md1))[1])
  margres<-mar[1:(nobs-1)]-rhohat*mar[1:(nobs-1)]
  tres<-t-rhohat*(t-1)
  betahat<-unname(coef(lm(margres~0+tres[1:(nobs-1)]))[1])
  m<-mar-betahat*t; dm<-c(0,diff(m)); s2<-mean(dm[1:(nobs-1)]^2)
  out$AMLMtrold<-amlm(rbind(m[1:(nobs-1)]),rbind(dm[2:nobs]),s2); out$rhotrold<-rhohat
  out
}

frac<-function(v,cr) sum(v>cr,na.rm=TRUE)/sum(!is.na(v))

pergame<-list(); fractab<-list(); crtab<-list()
for(m in seq_along(years)){
  yr<-years[m]
  d<-read.delim(paste0(base_path,yr,"/combined-stats.csv"),header=TRUE,sep=",",stringsAsFactors=FALSE)
  names(d)[1]<-"id"
  d<-d[-1,]
  d[d==""]<-NA
  d<-subset(d,event_type=="shot"|type=="free throw 2/2"|type=="free throw 1/1"|(event_type=="jump ball" & play_id==2))
  d$id<-as.numeric(d$id)
  d$team[is.na(d$team)]<-"0"
  d$home_score[is.na(d$home_score)]<-0; d$away_score[is.na(d$away_score)]<-0
  d$mar<-d$home_score-d$away_score
  if(yr %in% spreadyears){
    ts<-data.frame(read_excel(paste0(base_path,yr,"/Team-Stats.xlsx")))
    sp<-subset(ts,VENUE=="H",select=c(GAME.ID,OPENING.SPREAD,CLOSING.SPREAD,POSS))
    sp$GAME.ID<-as.numeric(as.character(sp$GAME.ID)); sp$POSS<-as.numeric(sp$POSS)
    sp$OPENING.SPREAD<-as.numeric(sp$OPENING.SPREAD); sp$CLOSING.SPREAD<-as.numeric(sp$CLOSING.SPREAD)
    sp<-sp[order(sp$GAME.ID),]
  }
  for(regular in c(1,0)){
    dd<-if(regular==1) d[d$id<30000000,] else d[d$id>30000000,]
    dd<-dd[order(dd$id),]
    ids<-unique(dd$id); games<-length(ids)
    tab<-if(regular==1) tabS6 else tabS7
    p<-tab[m,1:3]; cr<-tab[m,4:6]
    if(yr %in% spreadyears){
      spr<-if(regular==1) sp[sp$GAME.ID<30000000,] else sp[sp$GAME.ID>30000000,]
      bold<-(-1)*spr$OPENING.SPREAD/(2*spr$POSS)
      bold<-bold[1:games]               #row matching as in the original program
      idx<-match(ids,spr$GAME.ID)
      bnew<-(-1)*spr[[spreadvar]][idx]/spr$POSS[idx]
    } else {bold<-rep(NA,games); bnew<-rep(NA,games)}
    split_d<-split(dd,factor(dd$id,levels=ids))
    res<-vector("list",games)
    for(j in 1:games){
      g<-split_d[[j]]
      g<-g[g$team==g$team[1],]
      o<-oldstats(g$mar,bold[j],bnew[j])
      e<-esttrend(rbind(g$mar))
      res[[j]]<-data.table(season=yr,regular=regular,id=ids[j],date=g$date[1],
        teams=paste(sort(unique(g$team[g$team!="0"])),collapse="-"),nobs=nrow(g),
        AMLM=o$AMLM,rho=o$rho,AMLMspold=o$AMLMspold,AMLMspnew=o$AMLMspnew,rhospnew=o$rhospnew,
        AMLMtrold=o$AMLMtrold,AMLMtrnew=e$AMLM,rhotrnew=e$rho,betatrnew=e$betahat)
    }
    res<-rbindlist(res)
    #critical values for the corrected estimated trend
    Ms<-simnull(p)
    simtr<-esttrend(Ms)$AMLM
    crtr<-unname(quantile(simtr,c(0.9,0.95,0.99)))
    res[,ptrnew:=sapply(AMLMtrnew,function(a) mean(simtr>a))]
    pergame[[length(pergame)+1]]<-res
    crtab[[length(crtab)+1]]<-data.table(season=yr,regular=regular,crtr10=crtr[1],crtr05=crtr[2],crtr01=crtr[3])
    fractab[[length(fractab)+1]]<-res[,.(season=yr,regular=regular[1],games=.N,
      fr10=frac(AMLM,cr[1]),fr05=frac(AMLM,cr[2]),
      frd10old=frac(AMLMspold,cr[1]),frd05old=frac(AMLMspold,cr[2]),
      frd10=frac(AMLMspnew,cr[1]),frd05=frac(AMLMspnew,cr[2]),spreadgames=sum(!is.na(AMLMspnew)),
      frtr10old=frac(AMLMtrold,cr[1]),frtr05old=frac(AMLMtrold,cr[2]),
      frtr10=frac(AMLMtrnew,crtr[1]),frtr05=frac(AMLMtrnew,crtr[2]),
      frtr10_oldcr=frac(AMLMtrnew,cr[1]),frtr05_oldcr=frac(AMLMtrnew,cr[2]))]
    cat(yr,ifelse(regular==1,"regular","playoffs"),"done\n")
  }
}

pergame<-rbindlist(pergame); fractab<-rbindlist(fractab); crtab<-rbindlist(crtab)
fractab<-merge(fractab,crtab,by=c("season","regular"),sort=FALSE)
options(width=220,digits=4)
print(fractab)
fwrite(fractab,paste0(outdir,"AMLM fractions corrected ",spreadvar,".csv"))
fwrite(pergame,paste0(outdir,"AMLM by game corrected ",spreadvar,".csv"))
