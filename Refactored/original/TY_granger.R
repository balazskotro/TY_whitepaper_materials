source("stationarity_functions.R")

ts_differentiator<-function(df, signal)
{
  print(paste0("Evaluation was made based on ", signal, " test"))
  
  non_statinary_lambdas<-df
  max_diff_order<--1
  
  while ((nrow(non_statinary_lambdas) != 0)) {
    
    if (signal=='ADF')
    {
      result<-as.data.frame((sapply(df, adf.test)["p.value",]))
      #print(result)
      adf_t_stat<-as.data.frame((sapply(df, adf.test)["statistic",]))
      #print(adf_t_stat[1])
      if (max_diff_order==-1){
        write.csv(rbind(result,adf_t_stat),"raw_adf_t_stat.csv", row.names = TRUE)} else{
          write.csv(rbind(result[[1]],adf_t_stat[[1]]),paste0("I(",max_diff_order+1,")_adf_t_stat.csv"), row.names = TRUE) 
        }
      
      list_result<-lapply(colnames(result), function(x){adf_tester(result[x])})
      
      
    }else if (signal=='KPSS')
    {
      result<-as.data.frame((sapply(df, kpss.test)["p.value",]))
      print(result)
      kpss_t_stat<-as.data.frame((sapply(df, kpss.test)["statistic",]))
      print(kpss_t_stat[1])
      if (max_diff_order==-1){
        write.csv(rbind(result,kpss_t_stat),"raw_kpss_t_stat.csv", row.names = TRUE)} else{
          write.csv(rbind(result,kpss_t_stat),paste0("I(",max_diff_order+1,")_kpss_t_stat.csv"), row.names = TRUE) 
        }
      
      list_result<-lapply(colnames(result), function(x){kpss_tester(result[x])})
    }
    
    non_statinary_lambdas <- drop_na(data.frame(matrix(unlist(list_result), 
                                                       nrow=length(list_result), byrow=T))[2])
    
    if ((nrow(non_statinary_lambdas) == 0))
    {
      print("All the time series are statianary now")
      
    }else
    {
      #   while ((nrow(non_statinary_lambdas) == 0)) {
      #     
      #   }
      #   df_trunc<-df[-1,]
      #   df_trunc[c(t(non_statinary_lambdas))]
      #   
      to_diff<-as.data.frame(diff(ts(df[,c(t(non_statinary_lambdas))])))
      colnames(to_diff)<-c(t(non_statinary_lambdas))
      df[-1,colnames(to_diff)]<-diff(ts(df[,c(t(non_statinary_lambdas))]))
      df<-df[-1,]
      }
    
    #print(c(t(non_statinary_lambdas)))
    
    max_diff_order<-max_diff_order+1
    
    
    #print(paste0("Time-sereis ", (c(t(non_statinary_lambdas))), " was differentiated"))
  }
  cat(blue(paste("Max integration order is ", max_diff_order),"\n"))
  #print(paste("Max integration order is ", max_diff_order))
  return(list(df,max_diff_order))
}


result_list<-ts_differentiator(global_lambdas, type_of_test)
max_diff_order<-result_list[[2]]


global_lambdas<-result_list[[1]]

# coint_matrix<-as.data.frame(matrix(data= NA,
#                                     nrow = ncol(global_lambdas),
#                                     ncol = ncol(global_lambdas)))
# 
# 
# for (i in 1:nrow(coint_matrix))
# {
#   for (j in 1:ncol(coint_matrix))
#   {
# 
#     
#       
#     leg_longrun<-lm(as.matrix(global_lambdas[1])~as.matrix(global_lambdas[13]))
#       resid_reg_longrun=leg_longrun$residuals
#       y=ur.df(resid_reg_longrun,type="none",lags = 1, selectlags = "Fixed")
#       #summary(y)
#       y@teststat
#       
#       if (abs( y@teststat)>abs(y@cval[1])){
#       coint<-1
#     }else{coint<-0}
# 
#     coint_matrix[j,i]<-coint
# print(j)
#   }
#   print(paste0("i_",i))
# }
# 
# cointegrated_ratio<-sum(coint_matrix[upper.tri(coint_matrix)])/length(coint_matrix[upper.tri(coint_matrix)])
# 
# #factorial(nrow(coint_matrix))/(factorial(2)*factorial(nrow(coint_matrix)-2))
# 
# cat(cyan(paste(round(cointegrated_ratio*100,4),"% of the time series pairs are cointegrated\n")))

# var_res<-VARselect(global_lambdas[c(1,2)], lag=7, type = 'both')
# 
# if(Type_of_inf_criterion=="AIC")
# {
#   var_order<- var_res$selection[1] 
# }else if (Type_of_inf_criterion=="HQ")
# {
#   var_order<- var_res$selection[2]
# }else if (Type_of_inf_criterion=="SC")
# {
#   var_order<- var_res$selection[3]
# }else if (Type_of_inf_criterion=="FPE")
# {
#   var_order<- var_res$selection[4]
# }else
# {
#   print("Incorrect information Criterion!")
# }


# var_results<-VAR(global_lambdas[c(1,2)], p=var_order, type='both')
# if (1/roots(var_results)[[2]]>1){
#   cat(green("Stability analysis passes!\n"))
# } else{
#   cat(yellow("Warning: Stability analysis did not pass!\n"))  
# }
# 
# var_results_1<-VAR(global_lambdas,p=(var_order+max_diff_order), type='both')
# var_results_1$varresult
# summary(var_results_1)


result_matrix<-as.data.frame(matrix(data= NA, 
                                    nrow = ncol(global_lambdas), 
                                    ncol = ncol(global_lambdas)))

for (i in 1:nrow(result_matrix)) 
{
  for (j in 1:ncol(result_matrix)) 
  { 
    
    if (i==j) {
      j=j+1
    }
    
    if (j==37) {
      next
    }
    
    var_res<-VARselect(global_lambdas[c(j,i)], lag=7, type = 'both')
    
    if(Type_of_inf_criterion=="AIC")
    {
      var_order<- var_res$selection[1] 
    }else if (Type_of_inf_criterion=="HQ")
    {
      var_order<- var_res$selection[2]
    }else if (Type_of_inf_criterion=="SC")
    {
      var_order<- var_res$selection[3]
    }else if (Type_of_inf_criterion=="FPE")
    {
      var_order<- var_res$selection[4]
    }else
    {
      print("Incorrect information Criterion!")
    }
    
    
    var_results<-VAR(global_lambdas[c(j,i)], p=var_order, type='both')
    
    c_test<-causality(var_results,cause=colnames(global_lambdas[j]))$Granger 
    
    result_matrix[j,i]<-c_test$p.value
    
    print(i)
    
  }}

colnames(result_matrix)<-colnames(global_lambdas)
rownames(result_matrix)<-colnames(global_lambdas)


edge_counter  <- as.matrix(result_matrix)
diag(edge_counter)<-NA
edge_counter <- ifelse(abs(edge_counter) <= ((granger_p)), 1, 0)
diag(edge_counter)<-0

groups1   = list(1:((ncol(edge_counter))/3), 
                 ((ncol(edge_counter))/3+1):(((ncol(edge_counter))/3)*2), 
                 ((((ncol(edge_counter))/3)*2)+1):(ncol(edge_counter)))


edges_of_full_matrix<-sum(edge_counter)
outgoing_edges<-as.data.frame(apply(edge_counter, 1,sum))
incoming_edges<-as.data.frame(apply(edge_counter, 2,sum))
rownames(incoming_edges)<-paste0(rownames(incoming_edges),"_inner")
rownames(outgoing_edges)<-paste0(rownames(outgoing_edges),"_outer")

node_counter<-outgoing_edges+incoming_edges

L1edges<-sum(edge_counter[unlist(groups1[1]),unlist(groups1[1])])

L1edges_out<-sum(edge_counter[unlist(groups1[1]),unlist(groups1[2])])+sum(edge_counter[unlist(groups1[1]),unlist(groups1[3])])

max_edges_inner<-factorial(nrow(edge_counter[unlist(groups1[1]),unlist(groups1[1])]))/(factorial(2)*factorial(nrow(edge_counter[unlist(groups1[1]),unlist(groups1[1])])-2))
L2edges<-sum(edge_counter[unlist(groups1[2]),unlist(groups1[2])])

L2edges_out<-sum(edge_counter[unlist(groups1[2]),unlist(groups1[1])])+sum(edge_counter[unlist(groups1[2]),unlist(groups1[3])])

L3edges<-sum(edge_counter[unlist(groups1[3]),unlist(groups1[3])])                    
L3edges_out<-sum(edge_counter[unlist(groups1[3]),unlist(groups1[1])])+sum(edge_counter[unlist(groups1[3]),unlist(groups1[2])])

max_edges_outer<-factorial(nrow(edge_counter))/(factorial(2)*factorial(nrow(edge_counter)-2))-3*(factorial(nrow(edge_counter[unlist(groups1[1]),unlist(groups1[1])]))/(factorial(2)*factorial(nrow(edge_counter[unlist(groups1[1]),unlist(groups1[1])])-2)))

sum_inner_edges<-L1edges+L2edges+L3edges
sum_outer_edges<-L1edges_out+L2edges_out+L3edges_out
timeframe<-paste0(startperiod," - ",endperiod)
uscurve_positive<-uspos$US_positive[uspos$Date==endperiod]
#data<-cbind(timeframe,edges_of_full_matrix, sum_inner_edges,sum_inner_edges,L1edges,L2edges,L3edges,L1edges_out,L2edges_out,L3edges_out,t(incoming_edges),t(outgoing_edges))


