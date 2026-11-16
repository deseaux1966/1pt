#Corrected version of "PbPdata import score ratio test agg.R" and "... agg refFE.R" for Tables 1, 2 and S1-S5.
#Corrections:
# 1. Rest days: every code containing B2B (back to back, including 4IN5-B2B, 5IN6-B2B2B, B2B2B) is 0 days
#    of rest, and 3IN4, 4IN5 and 5IN6 are 1 day. The original coded 4IN5-B2B as 4, 5IN6-B2B2B as 5, etc.
# 2. The Q3 regressions without the spread use the sample filtered on mar123 and mar4 (the original
#    reused the Q2 sample).
#Rest Days and Won-Loss are road team minus home team, as in the original. Margins are home minus road.
#The spread is not demeaned (demean<-1 demeans it, as in the original), so the constant is the expected
#differential after the break for a game tied at the break with a spread of zero. Standard errors are HC2 (estimatr default) for models with
#controls, OLS for models without, and HC1 for the referee fixed effects models (fixest "hetero").
# 3. The spread is the closing spread, set just before the game, rather than the opening spread.
#Set fixrest=0, demean=1 and spreadvar="OPENING SPREAD" to reproduce the published tables.

library(readxl)
library(dplyr)
library(readr)

fixrest<-1
demean<-0
spreadvar<-"CLOSING SPREAD"   #"OPENING SPREAD" reproduces the published tables
years<-c("2021-2022","2020-2021","2019-2020","2018-2019","2017-2018","2016-2017","2015-2016",
         "2014-2015","2013-2014","2012-2013","2011-2012","2010-2011","2009-2010","2008-2009","2007-2008")
base_path<-"C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons/"
cols<-c("GAME-ID","TEAM","VENUE","1Q","2Q","3Q","4Q","OT1","OT2","OT3","OT4","OT5","F","REST",spreadvar,"MAIN REF")

dfteamstats<-NULL
for(y in years){
  d<-as.data.frame(read_excel(paste0(base_path,y,"/Team-Stats.xlsx"))[cols])
  d$W<-ifelse(d$VENUE=="R" & d$F>lead(d$F),1,0)
  d$W[d$VENUE=="H" & d$F>lag(d$F)]<-1
  d$L<-1-d$W
  d<-d %>% group_by(TEAM) %>% mutate(Ws=lag(cumsum(W),default=0),Ls=lag(cumsum(L),default=0)) %>% ungroup()
  dfteamstats<-bind_rows(dfteamstats,d)
}
d<-as.data.frame(dfteamstats)
names(d)[1]<-"id"; d$id<-as.numeric(d$id)
names(d)[names(d)==spreadvar]<-"spread"; d$spread<-as.numeric(d$spread); names(d)[names(d)=="MAIN REF"]<-"ref"
if(fixrest==1){
  d$rest<-ifelse(grepl("B2B",d$REST),0,ifelse(grepl("IN",d$REST),1,parse_number(d$REST)))
} else {
  d$rest<-parse_number(d$REST); d$rest[d$REST=="3IN4-B2B"]<-0; d$rest[d$REST=="B2B"]<-0; d$rest[d$REST=="3IN4"]<-1
}
d$restdiff<-lead(c(NA,-diff(d$rest)))
for(q in 1:4) d[[paste0("mar",q)]]<-lead(c(NA,diff(d[[paste0(q,"Q")]])))
d$mar12<-d$mar1+d$mar2; d$mar34<-d$mar3+d$mar4; d$mar123<-d$mar12+d$mar3; d$mar234<-d$mar2+d$mar34
d$WLdiff<-d$Ws-d$Ls-(lead(d$Ws)-lead(d$Ls))
dftsr<-subset(d,VENUE=="R")

#OLS with heteroskedasticity robust standard errors of type HC0, HC1 or HC2
rob<-function(f,data,type){
  m<-lm(f,data=data); X<-model.matrix(m); e<-resid(m); n<-nrow(X); k<-ncol(X)
  w<-switch(type,OLS=NA,HC1=e^2*n/(n-k),HC2=e^2/(1-hatvalues(m)))
  V<-if(type=="OLS") vcov(m) else {B<-solve(crossprod(X)); B%*%crossprod(X*sqrt(w))%*%B}
  se<-sqrt(diag(V)); t<-coef(m)/se
  list(b=coef(m),se=se,p=2*pt(-abs(t),df=n-k),adjr2=summary(m)$adj.r.squared)
}
breaks<-list(Q1=c("mar1","mar234"),Q2=c("mar12","mar34"),Q3=c("mar123","mar4"))
est<-function(s,controls,fe=FALSE,type){
  lapply(breaks,function(k){
    z<-s[!(s[[k[1]]]==0)|!(s[[k[2]]]==0),]
    z$marx<-z[[k[1]]]; z$mary<-z[[k[2]]]
    rhs<-paste(c("marx",controls,if(fe) "factor(ref)"),collapse="+")
    rob(as.formula(paste("mary~",rhs)),z,type)
  })
}
cell<-function(m,v){
  if(!(v %in% names(m$b))) return(c("",""))
  b<-m$b[v]; p<-m$p[v]
  st<-if(p<0.001) "^{***}" else if(p<0.01) "^{**}" else if(p<0.05) "^{*}" else ""
  c(sprintf("$%.3f%s$",b,st),sprintf("$(%.3f)$",m$se[v]))
}
textab<-function(models,vars,labels){
  out<-c()
  for(i in seq_along(vars)){
    cc<-sapply(models,cell,v=vars[i])
    out<-c(out,paste(labels[i],"&",paste(cc[1,],collapse=" & "),"\\\\"),paste("             &",paste(cc[2,],collapse=" & "),"\\\\"))
  }
  c(out,paste("Adj. R$^2$ &",paste(sprintf("$%.3f$",sapply(models,function(m) m$adjr2)),collapse=" & "),"\\\\"))
}

reg<-subset(dftsr,id<30000000)
pl<-subset(dftsr,id>30000000)
if(demean==1){reg$spread<-reg$spread-mean(reg$spread); pl$spread<-pl$spread-mean(pl$spread)}

vars<-c("marx","spread","restdiff","WLdiff","(Intercept)")
labs<-c("$\\hat{\\beta}_{0}$","Spread","Rest Days","Won-Loss","Constant")
srow<-function(s) sprintf("%.2f & %.2f & %.2f & %.2f",mean(s$spread),sd(s$spread),min(s$spread),max(s$spread))
cat("\nTable 1, Spread row:",srow(reg)," Table S2, Spread row:",srow(pl),"\n")
cat("\nTable 1, Rest Days row:",sprintf("%.2f & %.2f & %d & %d",mean(reg$restdiff,na.rm=TRUE),sd(reg$restdiff,na.rm=TRUE),
    as.integer(min(reg$restdiff,na.rm=TRUE)),as.integer(max(reg$restdiff,na.rm=TRUE))),"\n")
cat("\nTable 2\n"); writeLines(textab(c(est(reg,NULL,type="OLS"),est(reg,"spread",type="HC2")),vars,labs))
cat("\nTable S1\n"); writeLines(textab(c(est(reg,c("spread","restdiff","WLdiff"),type="HC2"),est(reg,c("restdiff","WLdiff"),type="HC2")),vars,labs))
cat("\nTable S3 (playoffs)\n"); writeLines(textab(c(est(pl,NULL,type="OLS"),est(pl,"spread",type="HC2")),vars,labs))
cat("\nTable S4 (referee fixed effects)\n"); writeLines(textab(c(est(reg,NULL,TRUE,"HC1"),est(reg,"spread",TRUE,"HC1")),vars[1:4],labs[1:4]))
cat("\nTable S5 (referee fixed effects)\n"); writeLines(textab(c(est(reg,c("spread","restdiff","WLdiff"),TRUE,"HC1"),est(reg,c("restdiff","WLdiff"),TRUE,"HC1")),vars[1:4],labs[1:4]))
cat("\nhead referees:",length(unique(reg$ref)),"\n")
