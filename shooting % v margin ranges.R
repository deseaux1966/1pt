#Set the working director where the file to be imported is.

library(sjmisc)

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

dfNBAha<-subset(dfNBA, dfNBA$id<0)
games<-1080  #the number of regular season games

ids<-(22000000+1):(22000000+1080)
for(j in 1:1080){
dfk<-subset(dfNBA,id==ids[j])
#The following loop coverts team names to "home" and "away".
k<-1
while(k<100){
  if (dfk[k,3]>0){
  break
  } 
  k=k+1
}
dfk$team[dfk$team==dfk[k,6]]<-"away"
dfk$team[dfk$team!=dfk[k,6]]<-"home"
dfNBAha<-rbind(dfNBAha,dfk)
}

dfNBAha$mar<-dfNBAha$home_score-dfNBAha$away_score
dfNBAha$mar<-c(0,dfNBAha$mar[1:239182])
dfNBAha$maradj<-ifelse(dfNBAha$team=="away",-dfNBAha$mar,dfNBAha$mar)
dfNBAha$typeadj<-ifelse(dfNBAha$type=="free throw 1/1" | dfNBAha$type=="free throw 1/2" | dfNBAha$type=="free throw 2/2"| dfNBAha$type=="free throw technical" | dfNBAha$type=="free throw flagrant 1/1" | dfk$type=="free throw flagrant 1/2" | dfNBAha$type=="free throw flagrant 2/2","ft",ifelse(dfNBAha$type=="3pt jump shot" | dfNBAha$type=="3pt pullup jump shot" | dfk$type=="3pt step back jump shot" | dfNBAha$type=="3pt running jump shot","3pt","2pt"))

marbar<-1
margin<-c(marbar*((-60/marbar):(60/marbar)))
shooting_2pt<-margin
shooting_3pt<-margin
shooting_ft<-margin
for (i in 1:length(margin)) {
  j<-margin[i]
  shooting_ft[i]<-nrow(dfNBAha[dfNBAha$maradj %in% j+0:(marbar-1) & dfNBAha$typeadj=="ft" & dfNBAha$result=="made",])/nrow(dfNBAha[dfNBAha$maradj %in% j+0:(marbar-1) & dfNBAha$typeadj=="ft",])
  shooting_2pt[i]<-nrow(dfNBAha[dfNBAha$maradj %in% j+0:(marbar-1) & dfNBAha$typeadj=="2pt" & dfNBAha$result=="made",])/nrow(dfNBAha[dfNBAha$maradj %in% j+0:(marbar-1) & dfNBAha$typeadj=="2pt",])
  shooting_3pt[i]<-nrow(dfNBAha[dfNBAha$maradj %in% j+0:(marbar-1) & dfNBAha$typeadj=="3pt" & dfNBAha$result=="made",])/nrow(dfNBAha[dfNBAha$maradj %in% j+0:(marbar-1) & dfNBAha$typeadj=="3pt",])
 }
shooting<-data.frame(margin,shooting_2pt,shooting_3pt,shooting_ft)
#plot(margin,shooting_2pt)
#plot(margin,shooting_3pt)
#plot(margin,shooting_ft)

plot(margin[31:90],shooting_3pt[31:90],yaxt="n",ylim=c(0.0,1.5),ylab="shooting probs",xlab="margin")
points(margin,shooting_2pt,pch="+")
points(margin,shooting_ft,pch="*")
axis(2, at = c(0,0.5,1.0))
legend("top",cex=0.9,legend=c("3 pt","2 pt","free throw"),pch=c("o","+","*"),horiz=TRUE)

