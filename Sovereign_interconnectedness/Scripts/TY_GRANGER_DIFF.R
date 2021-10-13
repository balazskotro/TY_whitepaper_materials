# adf_result<-as.data.frame((sapply(global_lambdas, adf.test)["p.value",]))
# 
# adf_result_l<-sapply(global_lambdas, adf.test)["p.value",]
#adf_df<-t(adf_df)

c_level=0.01

global_lambdas2<-global_lambdas[-1,]

checker<-function(df){
  
  adf_result<-as.data.frame((sapply(df, adf.test)["p.value",]))  
  
  kpss_result<-as.data.frame((sapply(df, kpss.test)["p.value",]))
  
  checker_adf<-function(adf_result){
    
    if (adf_result<=c_level){
      
      message<-(paste0("ADF: ", colnames(adf_result), " is stationary on level ", c_level))
      return(message)
      
    }else{
      return(paste0(colnames(df), " is not stationary on level ", c_level))
    }
  }
  checker_kpss<-function(kpss_result){
    if (kpss_result>c_level){
      
      #message1<-(paste0("KPSS: ", colnames(adf_result), " is stationary on level ", c_level))
      
    }else{
      #message1<- (paste0(colnames(df), " is not stationary on level ", c_level))
    }
    
  }
  
  message<-lapply(colnames(adf_result), function(x){checker_adf(adf_result[x])})
  
  
  newlist <- list(message)
  
  return(newlist)
  
}


checker(global_lambdas)


lapply(colnames(global_lambdas), function(x){checker(global_lambdas[x])})

# checker(adf_result[1])
# 
# lapply(adf_result[1,], checker)
# 
# 
# 
# trialData <- data.frame('a' = rnorm(100),
#                         'b' = rnorm(100),
#                         'c' = rnorm(100))
# 
# someData <- function(dataInput){
#   # lots of code here
#   return(dataName = colnames(dataInput))


dataOutput <- lapply(colnames(adf_result), function(x){checker(adf_result[x])})

print(dataOutput)

rm(list = ls()) #delete everything
df<-read.csv(file = 'global_lambdas.csv',row.names=1)
glob<-df
glob2<-glob[-1,]
#adf_result<-as.data.frame((sapply(df, adf.test)["p.value",])) 
#--------------------------------------------------------------------------------------------------



checker_adf<-function(data_for_adf,c_level=0.1){
  
  if (data_for_adf<=c_level){
    
    message_stat<-print(paste0("ADF: ", 
                               colnames(data_for_adf), " is stationary on level ", c_level))
    
    else_list <- list(message,NA)

  } else{
    
    message_non_stat<-print(paste0("ADF: ", 
                                   colnames(data_for_adf), " is not stationary on level ", c_level))
    
    lambda_non_stat<-colnames(data_for_adf)
  
    else_list<-list(message_non_stat,lambda_non_stat)
    
  
  }
  

}
 

checker_kpss<-function(data_for_kpss,c_level=0.1){
  
  if (data_for_kpss>c_level){
    
    message_stat<-print(paste0("KPSS: ", 
                               colnames(data_for_kpss), " is stationary on level ", c_level))
    
    else_list <- list(message,NA)
    
  } else{
    
    message_non_stat<-print(paste0("KPSS: ", 
                                   colnames(data_for_kpss), " is not stationary on level ", c_level))
    
    lambda_non_stat<-colnames(data_for_kpss)
    
    else_list<-list(message_non_stat,lambda_non_stat)
    
    
  }
  
  
}


#adf_list_result<-lapply(colnames(adf_result), function(x){checker_adf(adf_result[x])})

#non_statinary_lambdas <- drop_na(data.frame(matrix(unlist(adf_list_result), 
                                                   #nrow=length(adf_list_result), byrow=T))[2])



#glob2[c(t(non_statinary_lambdas))]<-diff(ts(glob[c(t(non_statinary_lambdas))]))



checker2<-function(df, signal="KPSS"){
  
  df_trunc<-df[-1,]
  
  if (signal=='ADF'){
  
  result<-as.data.frame((sapply(df, adf.test)["p.value",]))
  list_result<-lapply(colnames(result), function(x){checker_adf(result[x])})
  
  } else if (signal=='KPSS') {
  
    result<-as.data.frame((sapply(df, kpss.test)["p.value",]))
    list_result<-lapply(colnames(result), function(x){checker_kpss(result[x])})
  
  }
  
  
  non_statinary_lambdas <- drop_na(data.frame(matrix(unlist(list_result), 
                                                     nrow=length(list_result), byrow=T))[2])
  
  df_trunc[c(t(non_statinary_lambdas))]<-diff(ts(df[c(t(non_statinary_lambdas))]))
  
  return(df_trunc)
}

df1<-checker2(df, "KPSS")

