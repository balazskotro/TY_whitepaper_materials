#---
#Delete everything
rm(list = ls()) 

#Rewrite this to a proper working direction
options(stringsAsFactors = FALSE) 
setwd("C:\\Research\\Sovereign_interconnectedness\\Scripts")

#Call packages
pacman::p_load(stringi, chron, reshape2, plyr, ggrepel, directlabels, tidyr,
               dplyr, RODBC, ggplot2, grid, gridExtra, xlsx, YieldCurve, readxl,
               forecast, mtest, vars, sqldf, qgraph, fUnitRoots, urca, aod, zoo,
               tseries, gtools, stringr, tbl2xts, crayon, Cairo, data.table,
               reticulate, readr, egcm, pastecs, scales, vars, ggpubr) 


#---
#Input parameters
usage <- "excel"
frequency <- "daily" #daily weekly monthly
input <- c("all","node")
nodeident <- "USA_B_1"
typeoftest <- 'TY'
c_lev <- 0.01
type_of_test <- "ADF"
Type_of_inf_criterion <- "AIC" #AIC, HQ SC, FPE
granger_p <- 1/1000
time_subset <- "fix"
windowsize <- 750
ws <- 5
start_date <- as.Date('2004-07-01')
end_date <- as.Date('2019-12-31')

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
#Get data
source("TY_GRANGER_RAW_DATA2.R") 

#Tailor data according to desired frequency
if (frequency == "daily") {
  curve_universe_pivot <- curve_universe_pivot
  
  }else if (frequency == "weekly"){
  curve_universe_pivot <-curve_universe_pivot[
    weekdays(as.Date(curve_universe_pivot$Date)) == 'péntek',]
  
  } else if (frequency == "monthly"){
    curve_universe_pivot <- curve_universe_pivot[curve_universe_pivot$Date %in% 
                                  as.data.frame(
                                    as.Date(tapply(
                                      curve_universe_pivot$Date, substr(
                                        curve_universe_pivot$Date, 1, 7), 
                                      max)))[, 1], ]
  }
  
#---
#Calculate lambdas
source("TY_GRANGER_LAMBDA2.R")


runner=0
global_lambdas_ww<-global_lambdas

if (time_subset == "expanding") {
  global_lambdas_w <- global_lambdas
  l = which(rownames(global_lambdas_w) == start_date)
  
  for (i in which(rownames(global_lambdas_w) == start_date):
       (which(rownames(global_lambdas_w) == end_date) - windowsize)) 
    {
    global_lambdas <- global_lambdas_w[l:(i + windowsize), ]
    print(l)
    print(i)
    print(paste("Expanding run between", rownames(global_lambdas_w)[l],
                "and", rownames(global_lambdas_w)[i+windowsize]))
    
    if (typeoftest == 'TY') {
    source("TY_TY.R")
      
    }else{source("TY_granger.R")}
  }
  } else if  (time_subset == "rolling"){
  global_lambdas_w <- global_lambdas
  
  for (i in seq(which(rownames(global_lambdas_w) == start_date),
                (which(rownames(global_lambdas_w) == end_date) - windowsize),
                by=ws)) 
  {
    global_lambdas <- global_lambdas_w[i:(i + windowsize),]
    startperiod <- rownames(global_lambdas_w[i:(i + windowsize), ])[1]
    endperiod <- tail(rownames(global_lambdas_w[i:(i + windowsize), ]),1)
    print(paste("Rolling run between", rownames(global_lambdas_w)[i], "and",
                rownames(global_lambdas_w)[i + windowsize]))
    
    if (typeoftest == 'TY') {
      source("TY_TY.R") 
      
    }else{source("TY_granger.R")}
    
    source("TY_PLOT3.R")
    
    data <- cbind(timeframe,edges_of_full_matrix, sum_inner_edges, 
                  sum_outer_edges, L1edges, L2edges, L3edges, L1edges_out, 
                  L2edges_out, L3edges_out,
                  t(incoming_edges), t(outgoing_edges),uscurve_positive)
    data_m <- cbind(timeframe,edge_counter)
    unique_edges<-cbind(timeframe,printer)
    overlapping_edges<-cbind(timeframe,printerO)
    spearman_correlation<-cbind(timeframe,spearman)
    
    if (runner == 0){
     data1 <- data
     write.csv(data1, paste0("edge_data_", startperiod, "_", endperiod, "_.csv"), 
               row.names = FALSE)
     data1_m <- data_m
     write.csv(data1_m, paste0("edge_data_m_", startperiod, "_",endperiod, "_.csv"), 
               row.names = TRUE) 
     
     unique_edges_csv<-unique_edges
     write.csv(unique_edges_csv, paste0("unique_edges_", startperiod, "_", endperiod, "_.csv"), 
               row.names = FALSE)
     
     overlapping_edges_csv<-overlapping_edges
     write.csv(overlapping_edges_csv, paste0("overlapping_edges_", startperiod, "_", endperiod, "_.csv"), 
               row.names = FALSE)
     
     spearman_correlation_csv<-spearman_correlation
     write.csv(spearman_correlation_csv, paste0("spearman", startperiod, "_", endperiod, "_.csv"), 
               row.names = FALSE)
     
    } else {
      data1 <- rbind(data1,data)
      data1_m<- rbind(data1_m,data_m)
      unique_edges_csv <- rbind(unique_edges_csv, unique_edges)
      overlapping_edges_csv <- rbind(overlapping_edges_csv, overlapping_edges)
      spearman_correlation_csv <- rbind(spearman_correlation_csv, spearman_correlation)
    }
   runner = runner+1 
   print(runner)
  }
  
  write.csv(data1, "edge_data.csv", row.names = FALSE)
  write.csv(data1_m, "edge_data_m.csv", row.names = TRUE)
  write.csv(unique_edges_csv, "unique.csv", row.names = FALSE)
  write.csv(overlapping_edges_csv, "overlapping.csv", row.names = TRUE)
  write.csv(spearman_correlation_csv, "spearman.csv", row.names = TRUE)
  
  } else if (time_subset == "fix"){

    global_lambdas <- global_lambdas[which(rownames(global_lambdas) == start_date):
                                       (which(rownames(global_lambdas) == end_date)),]
    startperiod<-start_date
    endperiod<-end_date
    
    if (typeoftest == 'TY') {
      source("TY_TY.R") 
    }else{
      source("TY_granger.R")}
    
    print(paste("Fix run between", 
                rownames(global_lambdas[which(rownames(global_lambdas) == 
                                                start_date),]),
                "and", rownames(global_lambdas[which(
                  rownames(global_lambdas) == end_date),])))
    source("TY_PLOT3.R")
  }   
