#Set the working director where the file to be imported is.
#set reg=1 for regular season games, reg=0 for playoff games
library(sjmisc)
library(tseries)
library(readxl)
library(dplyr)
library(readr)
library(texreg)

regular=1  #set reg-1 for regualar season games, 0 for playoff games.

# dfNBA<-read.delim("combined-stats.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
# startid<-dfNBA[4,1]-1
teamstats<-read_excel("Team-Stats.xlsx")
dfteamstats<-data.frame(teamstats)


names(dfteamstats)[2]<-"id"
dfteamstats$id<-as.numeric(dfteamstats$id)

if(regular == 1) {
  dfteamstats<-subset(dfteamstats, dfteamstats$id<30000000)
  } else {
    dfteamstats<-subset(dfteamstats, dfteamstats$id>30000000)
 
}

ids<-unique(dfteamstats$id)
games<-length(ids)

dfteamstats$rest<-parse_number(dfteamstats$TEAM..REST.DAYS)

dfteamstats$mar1<-lead(c(NA, diff(dfteamstats$X1Q)))                   #these are margins for each quarter
dfteamstats$mar2<-lead(c(NA, diff(dfteamstats$X2Q)))
dfteamstats$mar3<-lead(c(NA, diff(dfteamstats$X3Q)))
dfteamstats$mar4<-lead(c(NA, diff(dfteamstats$X4Q)))
dfteamstats$mar12<-dfteamstats$mar1+dfteamstats$mar2  #this is the margin for the first half.
dfteamstats$mar23<-dfteamstats$mar2+dfteamstats$mar3
dfteamstats$mar34<-dfteamstats$mar3+dfteamstats$mar4
dfteamstats$mar123<-dfteamstats$mar1+dfteamstats$mar2+dfteamstats$mar3
dfteamstats$mar234<-dfteamstats$mar2+dfteamstats$mar3+dfteamstats$mar4

dftsr<-subset(dfteamstats,VENUE=="R")     #make the df one row per game
dftsr<-dftsr[ ,-c(10:14)]
dftsr<-dftsr[ ,-c(13:26)]

#Change so that regression variables are the same for all.

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14<-lm(dftsrtest$mary~dftsrtest$marx+0)
summary(reg14)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24<-lm(dftsrtest$mary~dftsrtest$marx+0)
summary(reg24)

dftsr34<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))    
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34<-lm(dftsrtest$mary~dftsrtest$marx+0)
summary(reg34)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13<-lm(dftsrtest$mary~dftsrtest$marx+0)
#summary(reg13)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23<-lm(dftsrtest$mary~dftsrtest$marx+0)
#summary(reg23)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))    
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12<-lm(dftsrtest$mary~dftsrtest$marx+0)
summary(reg12)

#Adding spreads as control variables

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14x1<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD)
summary(reg14x1)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0))    
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24x1<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD)
#summary(reg24x1)

dftsrtest<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))    
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34x1<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD)
summary(reg34x1)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13x1<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD)
#summary(reg13x1)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23x1<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD)
#summary(reg23x1)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))    
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12x1<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD)
summary(reg12x1)

#Adding spreads and rest days as control variables

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))  
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14x2<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD+dftsrtest$rest)
summary(reg14x2)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0))    
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24x2<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD+dftsrtest$rest)
#summary(reg24x2)

dftsrtest<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))    
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34x2<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD+dftsrtest$rest)
summary(reg34x2)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))   
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13x2<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD+dftsrtest$rest)
#summary(reg13x2)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0)) 
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23x2<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD+dftsrtest$rest)
#summary(reg23x2)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))    
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12x2<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$OPENING.SPREAD+dftsrtest$rest)
#summary(reg12x2)

#Adding rest days (only) as control variables

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar234
reg14x3<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$rest)
summary(reg14x3)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0))
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar34
reg24x3<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$rest)
#summary(reg24x3)

dftsrtest<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))
dftsrtest$marx<-dftsrtest$mar123
dftsrtest$mary<-dftsrtest$mar4
reg34x3<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$rest)
summary(reg34x3)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar23
reg13x3<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$rest)
#summary(reg13x3)

dftsrtest<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0))
dftsrtest$marx<-dftsrtest$mar12
dftsrtest$mary<-dftsrtest$mar3
reg23x3<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$rest)
#summary(reg23x3)

dftsrtest<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))
dftsrtest$marx<-dftsrtest$mar1
dftsrtest$mary<-dftsrtest$mar2
reg12x3<-lm(dftsrtest$mary~dftsrtest$marx+dftsrtest$rest)
#summary(reg12x3)


coef_map1<-list("dftsrtest$marx" = "Margin 2","dftsrtest$OPENING.SPREAD" = "Spread","(Intercept)" = "Constant")
coef_map2<-list("dftsrtest$marx" = "Margin 2","dftsrtest$OPENING.SPREAD" = "Spread","dftsrtest$rest" = "Rest Days", "(Intercept)" = "Constant")

models1<-list(reg14, reg24, reg34,
             reg14x1, reg24x1, reg34x1)
models2<-list(reg14x3, reg24x3, reg34x3,
              reg14x2, reg24x2, reg34x2)

texreg(models1,
       digits = 3,
       custom.coef.map = coef_map1,
       custom.model.names = rep(c("Q1", "Q2", "Q3"), 2),
       caption = "Estimates of the ratios between margins for divisions of the games at the end of quarters")

texreg(models2,
       digits = 3,
       custom.coef.map = coef_map2,
       custom.model.names = rep(c("Q1", "Q2", "Q3"), 2),
       caption = "Estimates of the ratios between margins for divisions of the games at the end of quarters")


#custom.header = list("No Controls" = 1:3,
# "Control X1" = 4:6,
# "Control X2" = 7:9),

# dftsr14<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar234==0))    #eliminates 0 margins
# reg14x2<-lm(dftsr14$mar1~dftsr14$mar234+dftsr14$rest)   #regression 
# #summary(reg14x2)
# 
# dftsr24<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar34==0))    
# reg24x2<-lm(dftsr24$mar12~dftsr24$mar34+dftsr24$rest)  
# #summary(reg24x2)
# 
# dftsr34<-subset(dftsr,!(dftsr$mar123==0)|!(dftsr$mar4==0))    
# reg34x2<-lm(dftsr34$mar123~dftsr34$mar4+dftsr34$rest) 
# #summary(reg34x2)
# 
# dftsr13<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar23==0))   
# reg13x2<-lm(dftsr13$mar1~dftsr13$mar23+dftsr13$rest)  
# #summary(reg13x2)
# 
# dftsr23<-subset(dftsr,!(dftsr$mar12==0)|!(dftsr$mar3==0)) 
# reg23x2<-lm(dftsr23$mar12~dftsr23$mar3+dftsr23$rest)  
# #summary(reg23x2)
# 
# dftsr12<-subset(dftsr,!(dftsr$mar1==0)|!(dftsr$mar2==0))    
# reg12x2<-lm(dftsr12$mar1~dftsr12$mar2+dftsr12$rest)  
# #summary(reg12x2)

