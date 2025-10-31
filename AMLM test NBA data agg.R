#Set the working director where the file to be imported is.
#set reg=1 for regular season games, reg=0 for playoff games. Turn off when using "combined programs"

library(sjmisc)
library(tseries)

#regular=1 

dfNBA<-read.delim("combined-stats.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

dfNBA <- subset(dfNBA, select = -c(data_set,date,a1,a2,a3,a4,a5,h1,h2,h3,h4,h5,remaining_time,elapsed,play_length,assist,block,entered,left,opponent,player,possession,reason,steal,shot_distance,original_x,original_y,converted_x,converted_y,description))
dfNBA <- dfNBA[-c(1), ]
dfNBA <- replace(dfNBA, dfNBA == "", NA)
dfNBA[is.na(dfNBA)]=0
dfNBA$num <- as.numeric(dfNBA$num)
dfNBA$outof <- as.numeric(dfNBA$outof)
names(dfNBA)[1]<-"id"
dfNBA$id <- as.numeric(dfNBA$id)
dfNBA<-dfNBA[0, ]     #to be used with aggregating data loop

seasons<-15                       #The past 15 seasons have spread data.
years <- c("2021-2022", "2020-2021", "2019-2020", "2018-2019", "2017-2018", "2016-2017", "2015-2016",
           "2014-2015", "2013-2014", "2012-2013", "2011-2012", "2010-2011", "2009-2010"
           , "2008-2009", "2007-2008", "2006-2007", "2005-2006", "2004-2005", "2003-2004", "2002-2003")
base_path <- "C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons/"

for (m in 1:seasons) {
  full_path <- paste0(base_path, years[m])
  setwd(full_path) 
  dfNBA1<-read.delim("combined-stats.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
    dfNBA1 <- subset(dfNBA1, select = -c(data_set,date,a1,a2,a3,a4,a5,h1,h2,h3,h4,h5,remaining_time,elapsed,play_length,assist,block,entered,left,opponent,player,possession,reason,steal,shot_distance,original_x,original_y,converted_x,converted_y,description))
  dfNBA1 <- dfNBA1[-c(1), ]
  dfNBA1 <- replace(dfNBA1, dfNBA1 == "", NA)
  dfNBA1[is.na(dfNBA1)]=0
  dfNBA1$num <- as.numeric(dfNBA1$num)
  dfNBA1$outof <- as.numeric(dfNBA1$outof)
  names(dfNBA1)[1]<-"id"
  dfNBA1$id <- as.numeric(dfNBA1$id)
    dfNBA<-bind_rows(dfNBA,dfNBA1)
}

dfNBA<-subset(dfNBA,dfNBA$event_type=="shot"|dfNBA$type=="free throw 2/2"|dfNBA$type=="free throw 1/1"|(dfNBA$event_type=="jump ball" & dfNBA$play_id==2))
#eliminates all events excepts shots and free throws and the initial jump ball

if(regular == 1) {
  dfNBAha<-subset(dfNBA, dfNBA$id<30000000)
} else {
  dfNBAha<-subset(dfNBA, dfNBA$id>30000000)
}

dfNBAha<-dfNBAha[(order(dfNBAha$id)), ]

dfNBAha$mar<-dfNBAha$home_score-dfNBAha$away_score

ids<-unique(dfNBAha$id)
games<-length(ids)

AMLMvec=vector(mode="numeric",length=games)
rhovec=vector(mode="numeric",length=games)
pvalvec=vector(mode="numeric",length=games)

#ids<-c((startid+1):(startid+706),(startid+708):(startid+971),startid+973,(startid+1231):(startid+1318))
#(startid+1):(startid+games)

for(j in 1:games){
dfk<-subset(dfNBAha,id==ids[j])
if(nrow(dfk)==0){
  AMLMvec[j]<-NA
  next
}

dfk$pos<-ifelse(dfk$team == dfk$team[1], 1, 0)
dfk<-subset(dfk,pos==1)

mar<-dfk$mar
nobs<-length(mar)
mar1<-mar[2:nobs]
reg<-lm(mar[1:nobs-1]~0+mar1)
rhohat<-summary(reg)$coefficients[1]
rhovec[j]<-rhohat

nobs<-length(mar)
dmar<-c(0,diff(mar))
sigsqhat<-sigma(reg)^2     #mean(dmar[2:nobs-1]^2)
#should this be taken from the regression?
kap<-sum(mar[1:nobs-1]*dmar[2:nobs])/(nobs*sigsqhat)
ind<-ifelse(kap<0,1,0)
AMLM<-ind*((sum(mar[1:nobs-1]*dmar[2:nobs]))^2)/(sigsqhat*sum(mar[1:nobs-1]^2))
+(sum((mar[1:nobs-1]^2)*((dmar[2:nobs]^2)-sigsqhat))^2)/(2*sigsqhat*(2*sum((mar[1:nobs-1]^4)*(dmar[2:nobs]^2))-sigsqhat*sum(mar[1:nobs-1]^4)))

if (AMLM<0) break

AMLMvec[j]=AMLM
pvalvec[j]=length(AMLMvecsim[AMLMvecsim>AMLMvec[j]])/trials                      
}

#hist(AMLMvec,breaks=4000,xlim=c(0,20))
dfAMLM<-data.frame(ids,AMLMvec,rhovec,pvalvec)
AMLMvecsort<-sort(AMLMvec,decreasing=TRUE)

#The following require critical values from simulation programs:
frac01<-length(AMLMvec[AMLMvec>crit[4]])/games
frac05<-length(AMLMvec[AMLMvec>crit[3]])/games
frac10<-length(AMLMvec[AMLMvec>crit[2]])/games
missedgames<-sum(is.na(AMLMvec))
#c(frac10, frac05, frac01)



