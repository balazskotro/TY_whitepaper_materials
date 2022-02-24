#---
#Delete everything
rm(list = ls()) 

#Rewrite this to a proper working direction
options(stringsAsFactors = FALSE) 
workingDirectory <- "C:\\Research\\Sovereign_interconnectedness\\Refactored"
setwd(workingDirectory)

#Call packages
pacman::p_load(stringi, chron, reshape2, plyr, ggrepel, directlabels, tidyr,
               dplyr, RODBC, ggplot2, grid, gridExtra, xlsx, YieldCurve, readxl,
               forecast, mtest, vars, sqldf, qgraph, fUnitRoots, urca, aod, zoo,
               tseries, gtools, stringr, tbl2xts, crayon, Cairo, data.table,
               reticulate, readr, egcm, pastecs, scales, vars, ggpubr) 


#Variable explanation
# - usage: where to pick up lambdas, excel (static) ...
#or other string (regenerate lambdas)
# - frequency: daily, weekly or monthly
# - input: which net to plot example: ...
#"all", "node", "curvature", "level", "slope", "level/curvature", ...
#"slope/curvature", "level/slope"
# - typeoftest: do a Toda-Yamamoto (TY) or Granger (granger) test
# - c_lev: confidence level of stationarity
# - type_of_test: type of stationarty test (ADF or KPSS)
# - Type_of_inf_criterion: AIC, HQ SC, FPE
# - granger_p: confidence level of causality 
# - time_subset: window type, fix, rolling, expanding
# - windowsize: size of the window if it rolling
# - ws: movement of the window if it is rolling
# - start_date: start of the examined time series
# - end_date: end of the examined time series

#---
#Input parameters
usage <- "excel"
frequency <- "daily" #daily weekly monthly
input <- c("all","node")
nodeIdent <- "USA_B_1"
typeOfCausalityTest <- 'TY'
confidenceLevel <- 0.01
typeOfUnitRootTest <- "ADF"
typeOfInformationCriterion <- "AIC" #AIC, HQ SC, FPE
grangerPValue <- 1/1000
timeSubset <- "fix"
windowSize <- 750
windowShift <- 5
startDate <- as.Date('2004-07-01')
endDate <- as.Date('2019-12-31')

#---
#Get data
source("rawDataFormatting.R") 

#---
#Calculate betas
source("betaCalculation.R")

#---
#Calculate interconnctedness between startDate and endDate
source("dynamicCausalityTest.R")