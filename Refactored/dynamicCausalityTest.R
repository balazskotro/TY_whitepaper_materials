
runner=0

if (timeSubset == "expanding") {
  startRowNumber = which(rownames(globalBetas) == startDate)
  endRowNumber = which(rownames(globalBetas) == endDate)
  
  for (i in startRowNumber :endRowNumber - windowSize) 
  {
    globalBetas <- globalBetas[startRowNumber:(i + windowSize), ]
    print(startRowNumber)
    print(i)
    print(paste("Expanding run between", rownames(globalBetas)[startRowNumber],
                "and", rownames(globalBetas)[i+windowSize]))
    
    if (typeOfTest == 'TY') {
      source("todaYamamotoCausality")
      
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
