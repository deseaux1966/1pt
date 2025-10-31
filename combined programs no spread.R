#This version is for the years 2002-2007 when spreads were not available.
#Runs multiple programs to generate results for scoring probs, critical AMLN values, 
# and fractions showing stationarity with and without the trend

#data avaliable for trend regressions starting 2007-2008

regular<-0  #set reg-1 for regualar season games, 0 for playoff games.
seasons=5  #There are 15 seasons with the spead available
results<-matrix(0,nrow=seasons,ncol=12)

dfAMLMagg <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), 
                      c("ids", "AMLMvec", "rhovec", "pvalvec"))
dfAMLMagg$ids <- as.numeric(dfAMLMagg$ids)
dfAMLMtrestagg <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
                           c("ids", "AMLMvec", "rhovec", "pvalvec","spreadvec"))
dfAMLMtrestagg$ids <- as.numeric(dfAMLMtrestagg$ids)

years <- c("2006-2007", "2005-2006", "2004-2005", "2003-2004", "2002-2003")
base_path <- "C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/data multiple seasons/"

for (m in 1:seasons) {
  full_path <- paste0(base_path, years[m])
  setwd(full_path) 
source("C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/R programs '25/PbPdata import make per shots test.R")
source("C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/R programs '25/GameSimAMLM.R")
source("C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/R programs '25/AMLM test NBA data.R")
source("C:/Users/gawater/OneDrive - IL State University/Time Series/Hoops/R programs '25/AMLMdetvar test NBA data est.R")
     results[m,]<-c(oneptpc, twoptpc, threeptpc, crit[2], crit[3], crit[4], frac10, frac05, frac01, frac10trest, frac05trest, frac01trest)

     dfAMLM$ids <- as.numeric(dfAMLM$ids)
     dfAMLMagg<-bind_rows(dfAMLMagg,dfAMLM)
     dfAMLMtrest$ids <- as.numeric(dfAMLMtrest$ids)
     dfAMLMtrestagg<-bind_rows(dfAMLMtrestagg,dfAMLMtrest)
}

print(results)
