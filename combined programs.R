#Runs multiple programs to generate results for scoring probs, critical AMLN values, 
# and fractions showing stationarity with and without the trend

#data avaliable for trend regressions starting 2007-2008

library(dplyr)

regular<-1  #set reg-1 for regualar season games, 0 for playoff games. 
seasons=15  #There are 15 seasons with the spread available
results<-matrix(0,nrow=seasons,ncol=15)
dfestresults<-list()
dfestresultssp<-list()
dfAMLMagg <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), 
               c("ids", "AMLMvec", "rhovec", "pvalvec"))
dfAMLMspagg <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
                      c("ids", "AMLMvec", "rhovec", "pvalvec","spreadvec"))
dfAMLMtrestagg <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
                        c("ids", "AMLMvec", "rhovec", "pvalvec","spreadvec"))
dfAMLMagg$ids <- as.numeric(dfAMLMagg$ids)
dfAMLMspagg$ids <- as.numeric(dfAMLMspagg$ids)
dfAMLMtrestagg$ids <- as.numeric(dfAMLMtrestagg$ids)

years <- c("2021-2022", "2020-2021", "2019-2020", "2018-2019", "2017-2018", "2016-2017", "2015-2016",
           "2014-2015", "2013-2014", "2012-2013", "2011-2012", "2010-2011", "2009-2010"
           , "2008-2009", "2007-2008", "2006-2007", "2005-2006", "2004-2005", "2003-2004", "2002-2003")
base_path <- "C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons/"

for (m in 1:seasons) {
  full_path <- paste0(base_path, years[m])
  setwd(full_path) 
source("C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/R programs '25/PbPdata import make per shots test.R")
source("C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/R programs '25/GameSimAMLM.R")
source("C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/R programs '25/AMLM test NBA data.R")
source("C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/R programs '25/AMLMdetvar test NBA data spread.R")
source("C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/R programs '25/AMLMdetvar test NBA data est.R")
  
  results[m,]<-c(oneptpc, twoptpc, threeptpc, crit[2], crit[3], crit[4], frac10, frac05, frac01, frac10tr, frac05tr, frac01tr, frac10trest, frac05trest, frac01trest)
  #dfestresults[[m]]<-dfAMLM
  #dfestresultssp[[m]]<-dfAMLMsp
  dfAMLM$ids <- as.numeric(dfAMLM$ids)
  dfAMLMsp$ids <- as.numeric(dfAMLMsp$ids)
  dfAMLMtrest$ids <- as.numeric(dfAMLMtrest$ids)
  dfAMLMagg<-bind_rows(dfAMLMagg,dfAMLM)
  dfAMLMspagg<-bind_rows(dfAMLMspagg,dfAMLMsp)
  dfAMLMtrestagg<-bind_rows(dfAMLMtrestagg,dfAMLMtrest)
}

#c(oneptpc, twoptpc, threeptpc, crit[2], crit[3], crit[4], frac10, frac05, frac01)
#c(oneptpc, twoptpc, threeptpc, crit[2], crit[3], crit[4], frac10, frac05, frac01, frac10tr, frac05tr, frac01tr, frac10trest, frac05trest, frac01trest)
