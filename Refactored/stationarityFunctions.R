adfTest<-function(dataForADF,confLevel = confidenceLevel) #This fuction collectc all those lambdas
{                                                 #which are non stationary on a predefined level 
                                                  #according to the ADF test
  if (dataForADF<=confLevel)                      #That level is defined on the main script
  {
    messageIfStationary<-print(paste0("ADF: ", 
                               colnames(dataForADF), " is stationary on level ", confLevel))
    
    answerList <- list(messageIfStationary,NA)
    
  }else
  {
    messageIfNonStationary<-print(paste0("ADF: ", 
                                   colnames(dataForADF), " is not stationary on level ", confLevel))
    
    betaNonStationary<-colnames(confLevel)
    
    answerList<-list(messageIfNonStationary,betaNonStationary)
  }
}

kpssTest<-function(dataForKPSS,confLevel = confidenceLevel) #Similar to the ADF test but this is a KPSS test
{
  
  if (dataForKPSS>confLevel)
  {
    messageIfStationary<-print(paste0("KPSS: ", 
                               colnames(dataForKPSS), " is stationary on level ", confLevel))
    
    answerList <- list(messageIfStationary,NA)
    
  }else
  {
    messageIfNonStationary<-print(paste0("KPSS: ", 
                                   colnames(dataForKPSS), " is not stationary on level ", confLevel))
    
    betaNonStationary<-colnames(dataForKPSS)
    
    answerList<-list(messageIfNonStationary,betaNonStationary)
  }
}

#The below function checks all the time series for stationarity. Gives a promt about the check
#If the TS is non-stationary, it gets differentiated and the original vector gets overrode
timeSeriesDifferentiator<-function(df, signal)
{
  print(paste0("Evaluation was made based on ", signal, " test"))
  
  nonStatinaryBetas<-df
  maxDifferenceOrder<- -1
  
  while ((nrow(nonStatinaryBetas) != 0)) {
    
    if (signal=='ADF')
    {
      result<-as.data.frame((sapply(df, adf.test)["p.value",]))
      adfTStatistic<-as.data.frame((sapply(df, adf.test)["statistic",]))
      if (maxDifferenceOrder==-1){
        write.csv(rbind(result,adfTStatistic),paste0(workingDirectory, "\\parsedValues\\rawADFTStatiscic.csv"), row.names = TRUE)} else{
          write.csv(rbind(result[[1]],adfTStatistic[[1]]),paste0(workingDirectory, "\\parsedValues\\I(",maxDifferenceOrder + 1,")ADFTStatiscic.csv"), row.names = TRUE) 
        }
      
      resultList<-lapply(colnames(result), function(x){adfTest(result[x])})
      
      
    }else if (signal=='KPSS')
    {
      result<-as.data.frame((sapply(df, kpss.test)["p.value",]))
      kpssTStatistic<-as.data.frame((sapply(df, kpss.test)["statistic",]))
      if (maxDifferenceOrder==-1){
        write.csv(rbind(result,adfTStatistic),paste0(workingDirectory, "\\parsedValues\\rawKPSSTStatiscic.csv"), row.names = TRUE)} else{
          write.csv(rbind(result[[1]],adfTStatistic[[1]]),paste0(workingDirectory, "\\parsedValues\\I(",maxDifferenceOrder + 1,")KPSSTStatiscic.csv"), row.names = TRUE) 
        }
      
      resultList<-lapply(colnames(result), function(x){kpssTest(result[x])})
    }
    
    nonStatinaryBetas <- as.data.frame(colnames(result)[!is.na(sapply(resultList, `[`, 2))])
    
    if ((nrow(nonStatinaryBetas) == 0))
    {
      print("All the time series are statianary now")
      
    }else
    {
      
      df<-diff(ts(df[,c(t(nonStatinaryBetas))]))}
    
    #print(c(t(non_statinary_lambdas)))
    
    maxDifferenceOrder<-maxDifferenceOrder+1
    
    
    #print(paste0("Time-sereis ", (c(t(non_statinary_lambdas))), " was differentiated"))
  }
  cat(blue(paste("Max integration order is ", maxDifferenceOrder),"\n"))
  #print(paste("Max integration order is ", max_diff_order))
  return(maxDifferenceOrder)
}