#Set the working director where the file to be imported is.
#set reg=1 for regular season games, reg=0 for playoff games

library(sjmisc)
library(tseries)
library(readxl)

#regular=0

dfNBA<-read.delim("combined-stats.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
#startid<-dfNBA[4,1]-1
# teamstats<-read_excel("Team-Stats.xlsx")
# dfteamstats<-data.frame(teamstats)
# dfspread<-subset(dfteamstats,dfteamstats$VENUE=="H")
# dfspread<-subset(dfspread,select=c(GAME.ID,OPENING.SPREAD,POSS))
# dfspread$b<-(-1)*dfspread$OPENING.SPREAD/(2*dfspread$POSS)
# dfspread$GAME.ID = as.numeric(as.character(dfspread$GAME.ID)) 
# dfspread<-dfspread[order(dfspread$GAME.ID),]
# dfspread<-data.frame(dfspread$GAME.ID,dfspread$OPENING.SPREAD,dfspread$POSS,dfspread$b)
#spread is the published spread before the game


dfNBA <- subset(dfNBA, select = -c(data_set,date,a1,a2,a3,a4,a5,h1,h2,h3,h4,h5,remaining_time,elapsed,play_length,assist,block,entered,left,opponent,player,possession,reason,steal,shot_distance,original_x,original_y,converted_x,converted_y,description))
dfNBA <- dfNBA[-c(1), ]
dfNBA[dfNBA == ""] <- NA
dfNBA[is.na(dfNBA)]=0
dfNBA$num <- as.numeric(dfNBA$num)
dfNBA$outof <- as.numeric(dfNBA$outof)

dfNBA<-subset(dfNBA,dfNBA$event_type=="shot"|dfNBA$type=="free throw 2/2"|dfNBA$type=="free throw 1/1"|(dfNBA$event_type=="jump ball" & dfNBA$play_id==2))
#eliminates all events excepts shots and free throws and the inital jump ball
names(dfNBA)[1]<-"id"

if(regular == 1) {
  dfNBAha<-subset(dfNBA, dfNBA$id<30000000)
  #dfspread<-subset(dfspread, dfspread.GAME.ID<30000000)
} else {
  dfNBAha<-subset(dfNBA, dfNBA$id>30000000)
  #dfspread<-subset(dfspread, dfspread.GAME.ID>30000000)
}

dfNBAha<-dfNBAha[(order(dfNBAha$id)), ]

                 # dfNBA<-subset(dfNBA, dfNBA$id<30000000) #only regular season games
# dfspread<-subset(dfspread, dfspread$GAME.ID<30000000) #only regular season games

#dfNBA<-subset(dfNBA, dfNBA$id>30000000) #only playoff games
#dfspread<-subset(dfspread, dfspread$GAME.ID>30000000) #only playoff games

dfNBAha$mar<-dfNBAha$home_score-dfNBAha$away_score

ids<-unique(dfNBAha$id)
games<-length(ids)

AMLMvec=vector(mode="numeric",length=games)
rhovec=vector(mode="numeric",length=games)
pvalvec=vector(mode="numeric",length=games)
betavec=vector(mode="numeric",length=games)
spreadvec=vector(mode="numeric",length=games)

#ids<-c((startid+1):(startid+706),(startid+708):(startid+971),startid+973,(startid+1231):(startid+1318))
  #(startid+1):(startid+games)
#dfbhatsp<-data.frame(ids)
#dfbhatsp$bhat<-0

for(j in 1:games){
dfk<-subset(dfNBAha,id==ids[j])

dfk$pos<-ifelse(dfk$team == dfk$team[1], 1, 0)
dfk<-subset(dfk,pos==1)

#mar is the detrended margin data
#used for the AMLM stat
#marg is the raw margin data used

marg<-dfk$mar

nobs<-length(marg)
dmarg<-c(0,diff(marg))
beta<-mean(dmarg)
t<-1:nobs
margdet<-marg-beta*t
margdet1<-margdet[2:nobs]
reg<-lm(margdet[1:nobs-1]~0+margdet1)
rhohat<-summary(reg)$coefficients[1]
marg1<-marg[2:nobs-1]
margres<-marg[1:nobs-1]-rhohat*marg1
tres<-t-rhohat*(t-1)
regres<-lm(margres~0+tres[1:nobs-1])
betahat<-summary(regres)$coefficients[1]
betavec[j]<-betahat
mar<-marg-betahat*t

nobs<-length(mar)
dmar<-c(0,diff(mar))

sigsqhat<-mean(dmar[2:nobs-1]^2)
kap<-sum(mar[1:nobs-1]*dmar[2:nobs])/(nobs*sigsqhat)
ind<-ifelse(kap<0,1,0)
AMLM<-ind*((sum(mar[1:nobs-1]*dmar[2:nobs]))^2)/(sigsqhat*sum(mar[1:nobs-1]^2))+(sum((mar[1:nobs-1]^2)*((dmar[2:nobs]^2)-sigsqhat))^2)/(2*sigsqhat*(2*sum((mar[1:nobs-1]^4)*(dmar[2:nobs]^2))-sigsqhat*sum(mar[1:nobs-1]^4)))
AMLMvec[j]=AMLM
pvalvec[j]=length(AMLMvecsim[AMLMvecsim>AMLMvec[j]])/trials
rhovec[j]<-rhohat
spreadvec[j]<-betahat*nobs
}

#hist(AMLMvec,breaks=40,xlim=c(0,20))
dfAMLMtrest<-data.frame(ids,AMLMvec,rhovec,pvalvec,betavec,spreadvec)
AMLMvecsort<-sort(AMLMvec,decreasing=TRUE)

frac01trest<-length(AMLMvec[AMLMvec>crit[4]])/games
frac05trest<-length(AMLMvec[AMLMvec>crit[3]])/games
frac10trest<-length(AMLMvec[AMLMvec>crit[2]])/games
#c(frac10, frac05, frac01, frac10tr, frac05tr, frac01tr)
