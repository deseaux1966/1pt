#Set the working director where the file to be imported is.
#Some notable games:
#[2015-05-14]-0041400236-HOU@LAC
#[2008-06-17]-0040700406-LAL@BOS
#[2014-05-31]-0041300316-SAS@OKC
#[2013-04-23]-0041200112-BOS@NYK
#[2013-04-20]-0041200161-GSW@DEN
#[2017-03-04]-0021600924-MEM@HOU
#[2021-11-04]-0022100120-UTA@ATL
#[2021-11-04]-0022100121-BOS@MIA
#[2021-12-29]-0022100522-OKC@PHX
#[2021-05-22]-0042000171-DAL@LAC
#[2013-04-27]-0041200123-IND@ATL
#[2013-04-25]-0041200103-MIA@MIL
#[2015-04-27]-0041400125-MIL@CHI    p-val = 0.01 with the spread but 0.12 without
#[2016-06-19]-0041500407-CLE@GSW   Cavs championship game 7
#[2022-04-16]-0042100161-DEN@GSW   max pval dif
#[2021-06-29]-0042000304-MIL@ATL    max pval dif with est trend

library(sjmisc)
library(tseries)
library(lubridate)
library(rvest)
library(stringr)
library(tidyverse)
library(readxl)

game<-"[2021-06-29]-0042000304-MIL@ATL"
season<-"2020-2021"
betaest<-0.237825     #for use inserting estimated trends
file<-paste0(game,".csv")

date <- str_extract(game, "\\[.*?\\]")  # "[2016-06-19]"
id <- str_extract(game, "\\d{10}")  # "0041500407"
teams <- str_extract(game, "[A-Z]{3}@[A-Z]{3}")  # "CLE@GSW"
idnum<-as.numeric(id)

base_path <- "C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons/"
full_path <- paste0(base_path, season)
setwd(full_path) 

#Set the working directory to the file for the correct season, and paste the game file title below.
dfNBA<-read.delim("combined-stats.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
dfNBA<-subset(dfNBA,dfNBA$game_id==idnum)

teamstats<-read_excel("Team-Stats.xlsx")
dfteamstats<-data.frame(teamstats)

dfNBA <- subset(dfNBA, select = -c(data_set,date,a1,a2,a3,a4,a5,h1,h2,h3,h4,h5,remaining_time,elapsed,play_length,assist,block,entered,left,opponent,player,possession,reason,steal,shot_distance,original_x,original_y,converted_x,converted_y,description))
dfNBA <- dfNBA[-c(1), ]
dfNBA[dfNBA == ""] <- NA
dfNBA[is.na(dfNBA)]=0
dfNBA$num <- as.numeric(dfNBA$num)
dfNBA$outof <- as.numeric(dfNBA$outof)

dfNBA<-subset(dfNBA,dfNBA$event_type=="shot"|dfNBA$type=="free throw 2/2"|dfNBA$type=="free throw 1/1"|(dfNBA$event_type=="jump ball" & dfNBA$play_id==2))

dfNBA$mar<-dfNBA$home_score-dfNBA$away_score
dfNBA$pos<-ifelse(dfNBA$team == dfNBA$team[1], 1, 0)
dfNBA<-subset(dfNBA,pos==1) #Makes one possession for attempts from both teams
dfNBA$poss<-cumsum(dfNBA$pos)

 

spread<-dfteamstats$OPENING.SPREAD[dfteamstats$GAME.ID==id & dfteamstats$VENUE=="R"]
nobs<-length(dfNBA$poss)
beta<-spread/nobs

ggplot(dfNBA,aes(x = poss, y = mar)) +
  geom_line(color="blue",lwd=0.5) +
  geom_abline(slope = beta, intercept = 0, color = "magenta", linetype = "dashed",lwd=0.4) +
  annotate("point", x = max(dfNBA$poss), y = beta * max(dfNBA$poss) + 0, 
           color = "magenta", size = 3) +
  annotate("text", x = max(dfNBA$poss), y = beta * max(dfNBA$poss) + 0, 
           label = "spread", hjust = 1.1, vjust = -0.2, color = "magenta") +
  geom_abline(slope = betaest, intercept = 0, color = "darkgreen", linetype = "dashed",lwd=0.4) +
  
  annotate("text", x = max(dfNBA$poss), y = betaest * max(dfNBA$poss) + 0, 
           label = "estimated trend", hjust = 1.1, vjust = 4, color = "darkgreen") +
  scale_x_continuous(limits = c(0, max(dfNBA$poss)), expand = expansion(mult = c(0, 0.01))) +
  labs(x = "Possessions", 
       y = "Margin", 
       title = paste(teams, date)) +
  theme_minimal()+
  theme(plot.title = element_text(size=10,hjust = 0.5))

#For finding values from the saved stats:
#dfAMLMaggPL[dfAMLMaggPL$ids==idnum,]

#estimates the AMLM stat


mar<-dfNBA$mar
#mar<-mar[1:86]   #used for stress testing the AMLM stat
nobs<-length(mar)
mar1<-mar[2:nobs]
reg<-lm(mar[1:nobs-1]~0+mar1)
rhohat<-summary(reg)$coefficients[1]

nobs<-length(mar)
dmar<-c(0,diff(mar))
sigsqhat<-sigma(reg)^2
kap<-sum(mar[1:nobs-1]*dmar[2:nobs])/(nobs*sigsqhat)
ind<-ifelse(kap<0,1,0)
AMLM<-ind*((sum(mar[1:nobs-1]*dmar[2:nobs]))^2)/(sigsqhat*sum(mar[1:nobs-1]^2))+(sum((mar[1:nobs-1]^2)*((dmar[2:nobs]^2)-sigsqhat))^2)/(2*sigsqhat*(2*sum((mar[1:nobs-1]^4)*(dmar[2:nobs]^2))-sigsqhat*sum(mar[1:nobs-1]^4)))
