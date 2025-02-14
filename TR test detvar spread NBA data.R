#Set the working director where the file to be imported is.

library(sjmisc)
library(tseries)

H<-4
a<-0.5
dfNBA<-read.delim("combined-stats.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

dfNBA <- subset(dfNBA, select = -c(data_set,date,a1,a2,a3,a4,a5,h1,h2,h3,h4,h5,remaining_time,elapsed,play_length,assist,block,entered,left,opponent,player,possession,reason,steal,shot_distance,original_x,original_y,converted_x,converted_y,description))
dfNBA <- dfNBA[-c(1), ]
dfNBA[dfNBA == ""] <- NA
dfNBA[is.na(dfNBA)]=0
dfNBA$num <- as.numeric(dfNBA$num)
dfNBA$outof <- as.numeric(dfNBA$outof)

dfNBA<-subset(dfNBA,dfNBA$event_type=="shot"|dfNBA$event_type=="free throw"|(dfNBA$event_type=="jump ball" & dfNBA$play_id==2))
     #eliminates all events excepts shots and free throws and the inital jump ball
names(dfNBA)[1]<-"id"

dfNBA$mar<-dfNBA$home_score-dfNBA$away_score

dfteamstats<-read.delim("Team-Stats.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
dfspread<-subset(dfteamstats,dfteamstats$VENUE=="H")
dfspread<-subset(dfspread,select=c("GAME.ID","OPENING.SPREAD","POSS"))
dfspread$b<-(-1)*dfspread$OPENING.SPREAD/(2*dfspread$POSS)
dfspread<-dfspread[order(dfspread$GAME.ID),]

games<-1080  #the number of regular season games
TRvec=vector(mode="numeric",length=games)

ids<-(22000000+1):(22000000+1080)
dfbhatsp<-data.frame(ids)
dfbhatsp$bhat<-0

for(j in 1:1080){
dfk<-subset(dfNBA,id==ids[j])

#mar is the detrended margin data
#used for the AMLM stat
#marg is the raw margin data used

marg<-dfk$mar
nobs<-length(marg)
t<-1:nobs
betahat<-dfspread[j,4]
dfbhatsp$bhat[j]<-betahat
mar<-marg-betahat*t

Y<-a/(a+mar^2)
rjlist<-acf(Y,lag.max=10,type="covariance",plot="False",demean="True")
rj<-rjlist$acf
w=c(1-(1:H)/(H+1))
TRvec[j]<-rj[1]+2*sum(rj[2:(H+1)]*w[1:H])
}

hist(TRvec,breaks=40,xlim=c(0,0.5))
