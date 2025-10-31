#Set the working director where the file to be imported is.
#set reg=1 for regular season games, reg=0 for playoff games
library(sjmisc)

regular=1   #use 1 for regular season games, 0 for playoff games

dfNBA<-read.delim("combined-stats.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
#startid<-dfNBA[4,1]-1     #The number is 2xx00000 where xx is the last two digits of the start year

dfNBA <- subset(dfNBA, select = -c(data_set,date,a1,a2,a3,a4,a5,h1,h2,h3,h4,h5,remaining_time,elapsed,play_length,assist,block,entered,left,opponent,player,possession,reason,steal,shot_distance,original_x,original_y,converted_x,converted_y,description))
dfNBA <- dfNBA[-c(1), ]
dfNBA[dfNBA == ""] <- NA
dfNBA[is.na(dfNBA)]=0
dfNBA$num <- as.numeric(dfNBA$num)
dfNBA$outof <- as.numeric(dfNBA$outof)

dfNBA<-subset(dfNBA,dfNBA$event_type=="shot"|dfNBA$type=="free throw 2/2"|dfNBA$type=="free throw 1/1"|(dfNBA$event_type=="jump ball" & dfNBA$play_id==2))

dfNBA$mar<-dfNBA$home_score-dfNBA$away_score

names(dfNBA)[1]<-"id"

if(regular == 1) {
  dfNBAha<-subset(dfNBA, dfNBA$id<30000000)
} else {
  dfNBAha<-subset(dfNBA, dfNBA$id>30000000)
}

#dfNBAha<-subset(dfNBA, dfNBA$id<30000000) #< means only regular season games
#dfNBAha<-subset(dfNBA, dfNBA$id>30000000) #> means only playoff games

ids<-unique(dfNBAha$id)
games<-length(ids)  #the number of games

#ids<-(startid+1):(startid+games)
for(j in 1:games){
dfk<-subset(dfNBA,id==ids[j])
if (nrow(dfk)==0){
  next
  }

#The following loop coverts team names to "home" and "away".
#k<-1
# while(k<100){
#    if (dfk[k,3]>0){
#   break
#   } 
#   k=k+1
# }
#dfk$team[dfk$team==dfk[k,6]]<-"away"
#dfk$team[dfk$team!=dfk[k,6]]<-"home"
dfk$pos<-ifelse(dfk$team == dfk$team[1], 1, 0)

     if(j == 1) {
       dfNBAcomb <- dfk
     } else {
       dfNBAcomb <- rbind(dfNBAcomb, dfk)
     }
}

dfNBAcomb <- dfNBAcomb[c(TRUE, diff(dfNBAcomb$pos) != 0), ] #Eliminates multiple shots in a possession
possessions<-length(dfNBAcomb$pos)

oneptmake<-sum(abs(diff(dfNBAcomb$mar))==1)
twoptmake<-sum(abs(diff(dfNBAcomb$mar))==2)
threeptmake<-sum(abs(diff(dfNBAcomb$mar))==3)

oneptpc<-oneptmake/possessions
twoptpc<-twoptmake/possessions
threeptpc<-threeptmake/possessions
c(oneptpc, twoptpc, threeptpc)
