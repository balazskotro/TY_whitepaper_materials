adf_tester<-function(data_for_adf,c_level=c_lev) #This fuction collectc all those lambdas
{                                                 #which are non stationary on a predefined level 
                                                  #according to the ADF test
  if (data_for_adf<=c_level)                      #That level is defined on the main script
  {
    message_stat<-print(paste0("ADF: ", 
                               colnames(data_for_adf), " is stationary on level ", c_level))
    
    answer_list <- list(message_stat,NA)
    
  }else
  {
    message_non_stat<-print(paste0("ADF: ", 
                                   colnames(data_for_adf), " is not stationary on level ", c_level))
    
    lambda_non_stat<-colnames(data_for_adf)
    
    answer_list<-list(message_non_stat,lambda_non_stat)
  }
}

kpss_tester<-function(data_for_kpss,c_level=c_lev) #Similar to the ADF test but this is a KPSS test
{
  
  if (data_for_kpss>c_level)
  {
    message_stat<-print(paste0("KPSS: ", 
                               colnames(data_for_kpss), " is stationary on level ", c_level))
    
    answer_list <- list(message_stat,NA)
    
  }else
  {
    message_non_stat<-print(paste0("KPSS: ", 
                                   colnames(data_for_kpss), " is not stationary on level ", c_level))
    
    lambda_non_stat<-colnames(data_for_kpss)
    
    answer_list<-list(message_non_stat,lambda_non_stat)
  }
}

#The below function checks all the time series for stationarity. Gives a promt about the check
#If the TS is non-stationary, it gets differentiated and the original vector gets overrode
ts_differentiator<-function(df, signal)
{
  print(paste0("Evaluation was made based on ", signal, " test"))
  
  if (signal=='ADF')
  {
    result<-as.data.frame((sapply(df, adf.test)["p.value",]))
    list_result<-lapply(colnames(result), function(x){adf_tester(result[x])})
    
    
  }else if (signal=='KPSS')
  {
    result<-as.data.frame((sapply(df, kpss.test)["p.value",]))
    list_result<-lapply(colnames(result), function(x){kpss_tester(result[x])})
  }
  
  non_statinary_lambdas <- drop_na(data.frame(matrix(unlist(list_result), 
                                                     nrow=length(list_result), byrow=T))[2])
  
  if ((nrow(non_statinary_lambdas) == 0))
  {
    print("All the time series are statianary now")
    df_trunc<-df
  }else
  {
    df_trunc<-df[-1,]
    df_trunc[c(t(non_statinary_lambdas))]<-diff(ts(df[c(t(non_statinary_lambdas))]))
    
    print(paste0("Time-sereis ", (c(t(non_statinary_lambdas))), " was differentiated"))
  }
  
  return(df_trunc)
}