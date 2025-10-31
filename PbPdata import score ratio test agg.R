#Set the working director where the file to be imported is.
#set reg=1 for regular season games, reg=0 for playoff games
library(sjmisc)
library(tseries)
library(readxl)
library(dplyr)
library(readr)
library(texreg)
library(estimatr)

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

dfteamstats$WLdiff<-dfteamstats$Ws-dfteamstats$Ls - (lead(dfteamstats$Ws)-lead(dfteamstats$Ls))

dftsr<-subset(dfteamstats,VENUE=="R")     #make the df one row per game
#Change so that regression variables mary and marx are the same for all.
dftsr$spread<-dftsr$spread-mean(dftsr$spread)   #demeans the spread data

#No constant:

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14nc<-lm(dftsrtest$mary~dftsrtest$marx+0)
summary(reg14nc)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24nc<-lm(dftsrtest$mary~dftsrtest$marx+0)
summary(reg24nc)

dftsr34<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))    
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34nc<-lm(dftsrtest$mary~dftsrtest$marx+0)
summary(reg34nc)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13nc<-lm(dftsrtest$mary~dftsrtest$marx+0)
#summary(reg13nc)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23nc<-lm(dftsrtest$mary~dftsrtest$marx+0)
#summary(reg23nc)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))    
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12nc<-lm(dftsrtest$mary~dftsrtest$marx+0)
summary(reg12nc)

#With a constant:

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14<-lm(dftsrtest$mary~dftsrtest$marx)
summary(reg14)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24<-lm(dftsrtest$mary~dftsrtest$marx)
summary(reg24)

dftsr34<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))    
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34<-lm(dftsrtest$mary~dftsrtest$marx)
summary(reg34)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13<-lm(dftsrtest$mary~dftsrtest$marx)
#summary(reg13)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23<-lm(dftsrtest$mary~dftsrtest$marx)
#summary(reg23)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))    
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12<-lm(dftsrtest$mary~dftsrtest$marx)
summary(reg12)

#Adding spreads as control variables, no constant:

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14x1nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+0)
summary(reg14x1nc)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0))    
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24x1nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+0)
summary(reg24x1nc)

dftsrtest<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))    
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34x1nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+0)
summary(reg34x1nc)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13x1nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+0)
#summary(reg13x1nc)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23x1nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+0)
#summary(reg23x1nc)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))    
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12x1nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+0)
summary(reg12x1nc)

#Adding spreads as control variables, with a constant

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14x1<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread)
summary(reg14x1)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0))    
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24x1<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread)
summary(reg24x1)

dftsrtest<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))    
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34x1<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread)
summary(reg34x1)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13x1<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread)
#summary(reg13x1)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23x1<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread)
#summary(reg23x1)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))    
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12x1<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread)
summary(reg12x1)

#Adding spreads and rest days as control variables, no constant

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))  
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14x2nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff+0)
summary(reg14x2nc)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0))    
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24x2nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff+0)
#summary(reg24x2nc)

dftsrtest<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))    
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34x2nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff+0)
summary(reg34x2nc)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13x2nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff+0)
#summary(reg13x2nc)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23x2nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff+0)
#summary(reg23x2nc)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))    
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12x2nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff+0)
#summary(reg12x2nc)

#Adding spreads and rest days and won-loss difference as control variables

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))  
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14x2<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg14x2)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0))    
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24x2<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
#summary(reg24x2)

dftsrtest<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))    
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34x2<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg34x2)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13x2<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
#summary(reg13x2)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23x2<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
#summary(reg23x2)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))    
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12x2<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$spread+dftsrtest$restdiff+dftsrtest$WLdiff)
#summary(reg12x2)

#Adding rest days (only) as control variables, no constant

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14x3nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff+0)
summary(reg14x3nc)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0))
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24x3nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff+0)
#summary(reg24x3nc)

dftsrtest<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34x3nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff+0)
summary(reg34x3nc)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13x3nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff+0)
#summary(reg13x3nc)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0))
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23x3nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff+0)
#summary(reg23x3nc)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12x3nc<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff+0)
#summary(reg12x3nc)

#Adding rest days (only) as control variables

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14x3<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg14x3)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0))
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24x3<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff)
#summary(reg24x3)

dftsrtest<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34x3<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff)
summary(reg34x3)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13x3<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff)
#summary(reg13x3)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0))
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23x3<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff)
#summary(reg23x3)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12x3<-lm_robust(dftsrtest$mary~dftsrtest$marx+dftsrtest$restdiff+dftsrtest$WLdiff)
#summary(reg12x3)


coef_map1<-list("dftsrtest$marx" = "Margin post-","dftsrtest$spread" = "Spread","(Intercept)" = "Constant")
coef_map2<-list("dftsrtest$marx" = "Margin post-","dftsrtest$spread" = "Spread",
                "dftsrtest$restdiff" = "Rest Days","dftsrtest$WLdiff" = "Won-Loss","(Intercept)" = "Constant")

models1<-list(reg14, reg24, reg34,
             reg14x1, reg24x1, reg34x1)
models2<-list(reg14x2, reg24x2, reg34x2,
              reg14x3, reg24x3, reg34x3)

texreg(models1,
       digits = 3,
       custom.coef.map = coef_map1,
       custom.model.names = rep(c("Q1", "Q2", "Q3"), 2),
       include.ci="FALSE",
       include.rsquared="FALSE",
       #include.adjrs="FALSE",
       include.nobs="FALSE",
       booktabs="TRUE",
       caption = "Estimates of equation () where the dependent variable is the margin before the break.")

texreg(models2,
       digits = 3,
       custom.coef.map = coef_map2,
       custom.model.names = rep(c("Q1", "Q2", "Q3"), 2),
       include.ci="FALSE",
       include.rsquared="FALSE",
       #include.adjrs="FALSE",
       include.nobs="FALSE",
       booktabs="TRUE",
       caption = "Estimates of equation () where the dependent variable is the margin before the break.")


