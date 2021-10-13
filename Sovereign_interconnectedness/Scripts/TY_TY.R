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
      write.csv(rbind(result,adf_t_stat),"C:\\Research\\Sovereign_interconnectedness\\Data\\raw_adf_t_stat.csv", row.names = TRUE)} else{
      write.csv(rbind(result[[1]],adf_t_stat[[1]]),paste0("C:\\Research\\Sovereign_interconnectedness\\Data\\I(",max_diff_order+1,")_adf_t_stat.csv"), row.names = TRUE) 
      }
    
    list_result<-lapply(colnames(result), function(x){adf_tester(result[x])})
    
    
  }else if (signal=='KPSS')
  {
    result<-as.data.frame((sapply(df, kpss.test)["p.value",]))
    print(result)
    kpss_t_stat<-as.data.frame((sapply(df, kpss.test)["statistic",]))
    print(kpss_t_stat[1])
    if (max_diff_order==-1){
      write.csv(rbind(result,kpss_t_stat),"C:\\Research\\Sovereign_interconnectedness\\Data\\raw_kpss_t_stat.csv", row.names = TRUE)} else{
        write.csv(rbind(result,kpss_t_stat),paste0("C:\\Research\\Sovereign_interconnectedness\\Data\\I(",max_diff_order+1,")_kpss_t_stat.csv"), row.names = TRUE) 
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
    df<-diff(ts(df[,c(t(non_statinary_lambdas))]))}
  
  #print(c(t(non_statinary_lambdas)))
    
    max_diff_order<-max_diff_order+1
    
    
    #print(paste0("Time-sereis ", (c(t(non_statinary_lambdas))), " was differentiated"))
  }
  cat(blue(paste("Max integration order is ", max_diff_order),"\n"))
  #print(paste("Max integration order is ", max_diff_order))
  return(max_diff_order)
}


  
max_diff_order<-ts_differentiator(global_lambdas, type_of_test)


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

var_res<-VARselect(global_lambdas, lag=7, type = 'both')

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


var_results<-VAR(global_lambdas, p=var_order, type='both')
if (1/roots(var_results)[[2]]>1){
cat(green("Stability analysis passes!\n"))
} else{
  cat(yellow("Warning: Stability analysis did not pass!\n"))  
}

var_results_1<-VAR(global_lambdas,p=(var_order+max_diff_order), type='both')
var_results_1$varresult
summary(var_results_1)


result_matrix<-as.data.frame(matrix(data= NA, 
                                    nrow = ncol(global_lambdas), 
                                    ncol = ncol(global_lambdas)))

for (i in 1:nrow(result_matrix)) 
{
  for (j in 1:ncol(result_matrix)) 
  { 
    causality_measure<-wald.test(b=coef(var_results_1$varresult[[i]]),
                     Sigma = vcov(var_results_1$varresult[[i]]),
                     Terms = c(j,j+(1:var_order)*nrow(currency_mapping)*3))
    

    
    result_matrix[j,i]<-causality_measure$result$chi2[3] 
    
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


L1<-edge_counter[unlist(groups1[1]),unlist(groups1[1])]

inner_edge_counter_L1<-as.data.frame(apply(L1, 1,sum))
inner_edge_counter_L1<-sort(inner_edge_counter_L1$`apply(L1, 1, sum)`,decreasing = TRUE)

L1edges_out<-sum(edge_counter[unlist(groups1[1]),unlist(groups1[2])])+sum(edge_counter[unlist(groups1[1]),unlist(groups1[3])])

max_edges_inner<-factorial(nrow(edge_counter[unlist(groups1[1]),unlist(groups1[1])]))/(factorial(2)*factorial(nrow(edge_counter[unlist(groups1[1]),unlist(groups1[1])])-2))

L2edges<-sum(edge_counter[unlist(groups1[2]),unlist(groups1[2])])
L2<-edge_counter[unlist(groups1[2]),unlist(groups1[2])]

inner_edge_counter_L2<-as.data.frame(apply(L2, 1,sum))
inner_edge_counter_L2<-sort(inner_edge_counter_L2$`apply(L2, 1, sum)`,decreasing = TRUE)

L2edges_out<-sum(edge_counter[unlist(groups1[2]),unlist(groups1[1])])+sum(edge_counter[unlist(groups1[2]),unlist(groups1[3])])




L3edges<-sum(edge_counter[unlist(groups1[3]),unlist(groups1[3])])
L3<-edge_counter[unlist(groups1[3]),unlist(groups1[3])]
L3edges_out<-sum(edge_counter[unlist(groups1[3]),unlist(groups1[1])])+sum(edge_counter[unlist(groups1[3]),unlist(groups1[2])])


inner_edge_counter_L3<-as.data.frame(apply(L3, 1,sum))
inner_edge_counter_L3<-sort(inner_edge_counter_L3$`apply(L3, 1, sum)`,decreasing = TRUE)

max_edges_outer<-factorial(nrow(edge_counter))/(factorial(2)*factorial(nrow(edge_counter)-2))-3*(factorial(nrow(edge_counter[unlist(groups1[1]),unlist(groups1[1])]))/(factorial(2)*factorial(nrow(edge_counter[unlist(groups1[1]),unlist(groups1[1])])-2)))

sum_inner_edges<-L1edges+L2edges+L3edges
sum_outer_edges<-L1edges_out+L2edges_out+L3edges_out
timeframe<-paste0(startperiod," - ",endperiod)
uscurve_positive<-uspos$US_positive[uspos$Date==endperiod]
#data<-cbind(timeframe,edges_of_full_matrix, sum_inner_edges,sum_inner_edges,L1edges,L2edges,L3edges,L1edges_out,L2edges_out,L3edges_out,t(incoming_edges),t(outgoing_edges))

L4=L1+L2+L3


only_three<-L4
only_three[only_three==1]<-0
only_three[only_three==2]<-0

toplot_l1<-(L1!=1)-(L1==L4)
toplot_l1[toplot_l1==1]<-0
toplot_l1[toplot_l1==-1]<-1

toplot_l2<-(L2!=1)-(L2==L4)
toplot_l2[toplot_l2==1]<-0
toplot_l2[toplot_l2==-1]<-1

toplot_l3<-(L3!=1)-(L3==L4)
toplot_l3[toplot_l3==1]<-0
toplot_l3[toplot_l3==-1]<-1


printer1<-toplot_l1==1
printer2<-toplot_l2==1
printer3<-toplot_l3==1
printer1_1<- paste0(rownames(printer1)[row(printer1)[which(printer1)]]," - ",colnames(printer1)[col(printer1)[which(printer1)]])
printer2_1<- paste0(rownames(printer2)[row(printer2)[which(printer2)]]," - ",colnames(printer2)[col(printer2)[which(printer2)]])
printer3_1<- paste0(rownames(printer3)[row(printer3)[which(printer3)]]," - ",colnames(printer3)[col(printer3)[which(printer3)]])

printer<-rbind(as.matrix(printer1_1),as.matrix(printer2_1),as.matrix(printer3_1))

printer_o<-only_three==3
printer1_o<- paste0(rownames(printer_o)[row(printer_o)[which(printer_o)]]," - ",colnames(printer_o)[col(printer_o)[which(printer_o)]])
printerO<-as.matrix(printer1_o)


rho1_2<-cor.test(inner_edge_counter_L1, inner_edge_counter_L2, method= "spearman")[["estimate"]][["rho"]]
rho1_3<-cor.test(inner_edge_counter_L1, inner_edge_counter_L3, method= "spearman")[["estimate"]][["rho"]]
rho2_3<-cor.test(inner_edge_counter_L2, inner_edge_counter_L3, method= "spearman")[["estimate"]][["rho"]]

L1_all<-edge_counter[1:12,]
L2_all<-edge_counter[13:24,]
L3_all<-edge_counter[25:36,]

all_edge_counter_L1<-as.data.frame(apply(L1_all, 1,sum))
all_edge_counter_L1<-sort(all_edge_counter_L1$`apply(L1_all, 1, sum)`,decreasing = TRUE)

all_edge_counter_L2<-as.data.frame(apply(L2_all, 1,sum))
all_edge_counter_L2<-sort(all_edge_counter_L2$`apply(L2_all, 1, sum)`,decreasing = TRUE)

all_edge_counter_L3<-as.data.frame(apply(L3_all, 1,sum))
all_edge_counter_L3<-sort(all_edge_counter_L3$`apply(L3_all, 1, sum)`,decreasing = TRUE)

rho1_2_all<-cor.test(all_edge_counter_L1, all_edge_counter_L2, method= "spearman")[["estimate"]][["rho"]]
rho1_3_all<-cor.test(all_edge_counter_L1, all_edge_counter_L3, method= "spearman")[["estimate"]][["rho"]]
rho2_3_all<-cor.test(all_edge_counter_L2, all_edge_counter_L3, method= "spearman")[["estimate"]][["rho"]]

spearman<-cbind(rho1_2,rho1_3,rho2_3,rho1_2_all,rho1_3_all,rho2_3_all)
