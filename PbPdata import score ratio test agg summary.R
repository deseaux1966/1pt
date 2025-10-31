#Set the working director where the file to be imported is.
#set reg=1 for regular season games, reg=0 for playoff games
library(sjmisc)
library(tseries)
library(readxl)
library(dplyr)
library(readr)
library(texreg)
library(stargazer)

regular=0  #set reg-1 for regualar season games, 0 for playoff games.

teamstats<-read_excel("Team-Stats.xlsx") 
#dfteamstats<-data.frame(teamstats)     #For single season analysis, use this line and comment out lines 15-30

dfteamstats1<-as.data.frame(teamstats[c("GAME-ID","TEAM","VENUE","1Q","2Q","3Q","4Q","OT1","OT2","OT3","OT4","OT5","F","REST","OPENING SPREAD")])
dfteamstats<-dfteamstats1[0, ]     #to be used with aggregating data loop

seasons<-15                       #The past 15 seasons have spread data.
years <- c("2021-2022", "2020-2021", "2019-2020", "2018-2019", "2017-2018", "2016-2017", "2015-2016",
           "2014-2015", "2013-2014", "2012-2013", "2011-2012", "2010-2011", "2009-2010"
           , "2008-2009", "2007-2008", "2006-2007", "2005-2006", "2004-2005", "2003-2004", "2002-2003")
base_path <- "C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons/"

for (m in 1:seasons) {
  full_path <- paste0(base_path, years[m])
  setwd(full_path) 
  teamstats<-read_excel("Team-Stats.xlsx")   #Use these two lines to get results for a single season.
  dfteamstats1<-as.data.frame(teamstats[c("GAME-ID","TEAM","VENUE","1Q","2Q","3Q","4Q","OT1","OT2","OT3","OT4","OT5","F","REST","OPENING SPREAD")])
  dfteamstats1$W<-ifelse(dfteamstats1$VENUE=="R" & dfteamstats1$F>lead(dfteamstats1$F),1,0)
  dfteamstats1$W[dfteamstats1$VENUE=="H" & dfteamstats1$F>lag(dfteamstats1$F)]<-1
  dfteamstats1$L<-1-dfteamstats1$W              
  dfteamstats1 <- dfteamstats1 %>%                #Generates data for Wins and Losses
    group_by(TEAM) %>%
    mutate(Ws = lag(cumsum(W), default = 0)) %>%
    mutate(Ls = lag(cumsum(L), default = 0)) %>%
    ungroup()
  dfteamstats<-bind_rows(dfteamstats,dfteamstats1)
}

names(dfteamstats)[1]<-"id"
dfteamstats$id<-as.numeric(dfteamstats$id)

if(regular == 1) {
  dfteamstats<-subset(dfteamstats, dfteamstats$id<30000000)
  } else {
    dfteamstats<-subset(dfteamstats, dfteamstats$id>30000000)

}

ids<-unique(dfteamstats$id)
games<-length(ids)

dfteamstats$rest<-parse_number(dfteamstats$REST)
dfteamstats$rest[dfteamstats$REST=="3IN4-B2B"]<-0
dfteamstats$rest[dfteamstats$REST=="B2B"]<-0
dfteamstats$rest[dfteamstats$REST=="3IN4"]<-1
dfteamstats$restdiff<-lead(c(NA, -diff(dfteamstats$rest)))
names(dfteamstats)[15]="spread"

dfteamstats$mar1<-lead(c(NA, diff(dfteamstats$'1Q')))                   #these are margins for each quarter
dfteamstats$mar2<-lead(c(NA, diff(dfteamstats$'2Q')))
dfteamstats$mar3<-lead(c(NA, diff(dfteamstats$'3Q')))
dfteamstats$mar4<-lead(c(NA, diff(dfteamstats$'4Q')))
dfteamstats$mar12<-dfteamstats$mar1+dfteamstats$mar2  #this is the margin for the first half.
dfteamstats$mar23<-dfteamstats$mar2+dfteamstats$mar3
dfteamstats$mar34<-dfteamstats$mar3+dfteamstats$mar4
dfteamstats$mar123<-dfteamstats$mar1+dfteamstats$mar2+dfteamstats$mar3
dfteamstats$mar234<-dfteamstats$mar2+dfteamstats$mar3+dfteamstats$mar4

dfteamstats$WLdiff<-dfteamstats$Ws-dfteamstats$Ls - (lead(dfteamstats$Ws)-lead(dfteamstats$Ls))

dftsr<-subset(dfteamstats,VENUE=="R")     #make the df one row per game


stargazer(dftsr[c("spread","restdiff","WLdiff")],
          type = "latex",
          digits = 2,
          summary = TRUE,
          omit.summary.stat = "n",
          title = "Summary Statistics")

stargazer(dftsr[c("mar1","mar2", "mar3", "mar4")],
          type = "latex",
          digits = 2,
          summary = TRUE,
          omit.summary.stat = "n",
          title = "Summary Statistics")

stargazer(dftsr[c("mar12","mar34")],
          type = "latex",
          digits = 2,
          summary = TRUE,
          omit.summary.stat = "n",
          title = "Summary Statistics")

#var.test(dftsr$mar12,dftsr$mar34) test for variances across halfs
