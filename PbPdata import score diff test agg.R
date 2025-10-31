#Set the working director where the file to be imported is.
#set reg=1 for regular season games, reg=0 for playoff games
library(sjmisc)
library(tseries)
library(readxl)
library(dplyr)
library(readr)
library(texreg)
library(nortest)
library(lmtest)
library(estimatr)

regular=1  #set reg-1 for regualar season games, 0 for playoff games.

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
names(dfteamstats)[15]="spread"
dfteamstats$restdiff<-lead(c(NA, -diff(dfteamstats$rest)))

dfteamstats$mar1<-lead(c(NA, diff(dfteamstats$'1Q')))                   #these are margins for each quarter
dfteamstats$mar2<-lead(c(NA, diff(dfteamstats$'2Q')))
dfteamstats$mar3<-lead(c(NA, diff(dfteamstats$'3Q')))
dfteamstats$mar4<-lead(c(NA, diff(dfteamstats$'4Q')))
dfteamstats$mar12<-dfteamstats$mar1+dfteamstats$mar2  #this is the margin for the first half.
dfteamstats$mar23<-dfteamstats$mar2+dfteamstats$mar3
dfteamstats$mar34<-dfteamstats$mar3+dfteamstats$mar4
dfteamstats$mar123<-dfteamstats$mar1+dfteamstats$mar2+dfteamstats$mar3
dfteamstats$mar234<-dfteamstats$mar2+dfteamstats$mar3+dfteamstats$mar4

dfteamstats$mar1adj<-lead(c(NA, diff(dfteamstats$'1Q')))-(0.25*dfteamstats$spread)                   #these are margins for each quarter adjust for the spread
dfteamstats$mar2adj<-lead(c(NA, diff(dfteamstats$'2Q')))-(0.25*dfteamstats$spread)
dfteamstats$mar3adj<-lead(c(NA, diff(dfteamstats$'3Q')))-(0.25*dfteamstats$spread)
dfteamstats$mar4adj<-lead(c(NA, diff(dfteamstats$'4Q')))-(0.25*dfteamstats$spread)
dfteamstats$mar12adj<-dfteamstats$mar1+dfteamstats$mar2  #this is the margin for the first half.
dfteamstats$mar23adj<-dfteamstats$mar2adj+dfteamstats$mar3adj
dfteamstats$mar34adj<-dfteamstats$mar3adj+dfteamstats$mar4adj
dfteamstats$mar123adj<-dfteamstats$mar1adj+dfteamstats$mar2adj+dfteamstats$mar3adj
dfteamstats$mar234adj<-dfteamstats$mar2adj+dfteamstats$mar3adj+dfteamstats$mar4adj

dfteamstats$WLdiff<-dfteamstats$Ws-dfteamstats$Ls - (lead(dfteamstats$Ws)-lead(dfteamstats$Ls))

dftsr<-subset(dfteamstats,VENUE=="R")     #make the df one row per game
#dftsr<-subset(dftsr,dftsr$Ws+dftsr$Ls>20)    #eliminating the first 20 games of each season to reduce the variation in W-L
dftsr$spread<-dftsr$spread-mean(dftsr$spread)   #demeans the spread data

dftsrtest<-dftsr

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar234, dftsr$mar234 - dftsr$mar1)
reg14<-lm_robust(dftsrtest$dif~1)
summary(reg14)

dftsrtest$dif <- ifelse(dftsr$mar12 < 0, dftsr$mar12 - dftsr$mar34, dftsr$mar34 - dftsr$mar12)
#dftsrtest<-subset(dftsrtest,abs(dftsrtest$mar12)>10)      #restricts sample to large intitial margins
reg24<-lm_robust(dftsrtest$dif~1)
summary(reg24)

dftsrtest$dif <- ifelse(dftsr$mar123 < 0, dftsr$mar123 - dftsr$mar4, dftsr$mar4 - dftsr$mar123)
reg34<-lm_robust(dftsrtest$dif~1)
summary(reg34)

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar23, dftsr$mar23 - dftsr$mar1)
reg13<-lm_robust(dftsrtest$dif~1)
summary(reg13)

dftsrtest$dif <- ifelse(dftsr$mar12 < 0, dftsr$mar12 - dftsr$mar3, dftsr$mar3 - dftsr$mar12)
reg23<-lm_robust(dftsrtest$dif~1)
summary(reg23)

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar2, dftsr$mar2 - dftsr$mar1)
reg12<-lm_robust(dftsrtest$dif~1)
summary(reg12)


#Adding spreads as control variables:

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar234, dftsr$mar234 - dftsr$mar1)
reg14x1<-lm_robust(dftsrtest$dif~dftsrtest$spread)
summary(reg14x1)

dftsrtest$dif <- ifelse(dftsr$mar12 < 0, dftsr$mar12 - dftsr$mar34, dftsr$mar34 - dftsr$mar12)
reg24x1<-lm_robust(dftsrtest$dif~dftsrtest$spread)
summary(reg24x1)

dftsrtest$dif <- ifelse(dftsr$mar123 < 0, dftsr$mar123 - dftsr$mar4, dftsr$mar4 - dftsr$mar123)
reg34x1<-lm_robust(dftsrtest$dif~dftsrtest$spread)
summary(reg34x1)

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar23, dftsr$mar23 - dftsr$mar1)
reg13x1<-lm_robust(dftsrtest$dif~dftsrtest$spread)
summary(reg13x1)

dftsrtest$dif <- ifelse(dftsr$mar12 < 0, dftsr$mar12 - dftsr$mar3, dftsr$mar3 - dftsr$mar12)
reg23x1<-lm_robust(dftsrtest$dif~dftsrtest$spread)
summary(reg23x1)

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar2, dftsr$mar2 - dftsr$mar1)
reg12x1<-lm_robust(dftsrtest$dif~dftsrtest$spread)
summary(reg12x1)


#Adding spreads and rest days and W-L as control variables

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar234, dftsr$mar234 - dftsr$mar1)
reg14x2<-lm_robust(dftsrtest$dif~dftsrtest$spread+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg14x2)

dftsrtest$dif <- ifelse(dftsr$mar12 < 0, dftsr$mar12 - dftsr$mar34, dftsr$mar34 - dftsr$mar12)
reg24x2<-lm_robust(dftsrtest$dif~dftsrtest$spread+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg24x2)

dftsrtest$dif <- ifelse(dftsr$mar123 < 0, dftsr$mar123 - dftsr$mar4, dftsr$mar4 - dftsr$mar123)
reg34x2<-lm_robust(dftsrtest$dif~dftsrtest$spread+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg34x2)

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar23, dftsr$mar23 - dftsr$mar1)
reg13x2<-lm_robust(dftsrtest$dif~dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg13x2)

dftsrtest$dif <- ifelse(dftsr$mar12 < 0, dftsr$mar12 - dftsr$mar3, dftsr$mar3 - dftsr$mar12)
reg23x2<-lm_robust(dftsrtest$dif~dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg23x2)

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar2, dftsr$mar2 - dftsr$mar1)
reg12x2<-lm_robust(dftsrtest$dif~dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg12x2)


#Adding rest days and W-L (only) as control variables

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar234, dftsr$mar234 - dftsr$mar1)
reg14x3<-lm_robust(dftsrtest$dif~dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg14x3)

dftsrtest$dif <- ifelse(dftsr$mar12 < 0, dftsr$mar12 - dftsr$mar34, dftsr$mar34 - dftsr$mar12)
reg24x3<-lm_robust(dftsrtest$dif~dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg24x3)

dftsrtest$dif <- ifelse(dftsr$mar123 < 0, dftsr$mar123 - dftsr$mar4, dftsr$mar4 - dftsr$mar123)
reg34x3<-lm_robust(dftsrtest$dif~dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg34x3)

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar23, dftsr$mar23 - dftsr$mar1)
reg13x3<-lm_robust(dftsrtest$dif~dftsrtest$restdiff)
summary(reg13x3)

dftsrtest$dif <- ifelse(dftsr$mar12 < 0, dftsr$mar12 - dftsr$mar3, dftsr$mar3 - dftsr$mar12)
reg23x3<-lm_robust(dftsrtest$dif~dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg23x3)

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1 - dftsr$mar2, dftsr$mar2 - dftsr$mar1)
reg12x3<-lm_robust(dftsrtest$dif~dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg12x3)

#Estimations with margins adjusted with the Spread
dftsrtest$dif <- ifelse(dftsr$mar1adj < 0, dftsr$mar1adj - dftsr$mar234adj, dftsr$mar234adj - dftsr$mar1adj)
reg14adj<-lm_robust(dftsrtest$dif~1)
summary(reg14adj)

dftsrtest$dif <- ifelse(dftsr$mar12adj < 0, dftsr$mar12adj - dftsr$mar34, dftsr$mar34adj - dftsr$mar12adj)
#dftsrtest<-subset(dftsrtest,abs(dftsrtest$mar12adj)>10)      #restricts sample to large intitial margins
reg24adj<-lm_robust(dftsrtest$dif~1)
summary(reg24adj)

dftsrtest$dif <- ifelse(dftsr$mar123adj < 0, dftsr$mar123adj - dftsr$mar4adj, dftsr$mar4adj - dftsr$mar123adj)
reg34adj<-lm_robust(dftsrtest$dif~1)
summary(reg34adj)

dftsrtest$dif <- ifelse(dftsr$mar1 < 0, dftsr$mar1adj - dftsr$mar23adj, dftsr$mar23adj - dftsr$mar1adj)
reg13adj<-lm_robust(dftsrtest$dif~1)
summary(reg13adj)

dftsrtest$dif <- ifelse(dftsr$mar12adj < 0, dftsr$mar12adj - dftsr$mar3adj, dftsr$mar3adj - dftsr$mar12adj)
reg23adj<-lm_robust(dftsrtest$dif~1)
summary(reg23adj)

dftsrtest$dif <- ifelse(dftsr$mar1adj < 0, dftsr$mar1adj - dftsr$mar2adj, dftsr$mar2adj - dftsr$mar1adj)
reg12adj<-lm_robust(dftsrtest$dif~1)
summary(reg12adj)



#Setting up Tables:
coef_map1<-list("(Intercept)" = "Constant","dftsrtest$spread" = "Spread")
coef_map2<-list("(Intercept)" = "Constant","dftsrtest$spread" = "Spread","dftsrtest$restdiff" = "Rest Days","dftsrtest$WLdiff"="Won-Loss")

models1<-list(reg14, reg24, reg34,
             reg14x1, reg24x1, reg34x1)
models2<-list(reg14x3, reg24x3, reg34x3,
              reg14x2, reg24x2, reg34x2)

texreg(models1,
       digits = 3,
       custom.coef.map = coef_map1,
       custom.model.names = rep(c("Q1", "Q2", "Q3"), 2),
       include.ci="FALSE",
       include.rsquared="FALSE",
       include.adjrs="FALSE",
       include.nobs="FALSE",
       booktabs="TRUE",
       caption = "Estimates of the average difference between margins before and after the division at the end of quarters for each column.")

texreg(models2,
       digits = 3,
       custom.coef.map = coef_map2,
       custom.model.names = rep(c("Q1", "Q2", "Q3"), 2),
       include.ci="FALSE",
       include.rsquared="FALSE",
       include.adjrs="FALSE",
       include.nobs="FALSE",
       booktabs="TRUE",
       caption = "Estimates of the average difference between margins before and after the division at the end of quarters for each column.")


