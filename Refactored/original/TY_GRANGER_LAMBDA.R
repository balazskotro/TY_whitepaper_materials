myFun <- function(data) {
  temp1 <- sapply(data, is.list)
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names=FALSE)))
  cbind(data[!temp1], temp2)
}

#This is the NS Equation by Diebold and Li (2006)
nelson_siegel_calculate<-function(theta,lambda,beta0,beta1,beta2)
  {
  beta0 + beta1*(1-exp(-lambda * theta))/(lambda * theta) + 
    beta2*((1-exp(-lambda * theta))/(lambda * theta) - exp(-lambda * theta))
  }
#--------------------------------------------------------------------------------------------------

#This is a minimzer function which minimizes the erros between estimated and observed yield
min.ns <- function(data, param)
  {
  with(data, sum((yield - nelson_siegel_calculate(maturity, 0.609, param[1], param[2], param[3]))^2))
  }
#--------------------------------------------------------------------------------------------------


#This function calculates the lambdas
lambda_function<-function(yield_1, data_frame)
  {
  

  
  yldmat <- structure(list(maturity
                           ,
                           yield=yield_1), class = "data.frame",row.names = c(NA, -ncol(data_frame)))
  
  optimizer_results<-optim(par = c(1,1,1), fn = min.ns, data = yldmat)
 
  lambda_frame<-optimizer_results$par
}


# if (str_sub(colnames(country_df2),-5,-5)=='M'){
#   maturity = as.numeric(gsub("[^0-9.-]", "", colnames(data_frame)))/12 
# } else{
#   maturity = as.numeric(gsub("[^0-9.-]", "", colnames(data_frame)))
# }






#--------------------------------------------------------------------------------------------------
currency_mapping<-as.data.frame(unique(substr(names(curve_universe_pivot[-1]),1,2)))
for (i in 1:nrow(currency_mapping)) #iterate for all countries
  { 
  
  print("Calculating lambdas...")
  print(paste0(i," / ",nrow(currency_mapping)," -- ",currency_mapping[i,1]))

  country_df<-curve_universe_pivot%>% #select the yield series of current country
    dplyr::select(Date, starts_with(currency_mapping[i,1]))

  

  
  country_df2<-myFun(country_df)  
  #country_df<-as.data.frame(country_df)  #create a dataframe
colnames(country_df2)<-colnames(country_df)
  
  country_df2 <- xts(country_df2[,-1], order.by=as.Date(country_df2[,1])) #create an xts object
  
  for (j in 1: ncol(country_df2)){ 
    
    if (str_sub(colnames(country_df2[,j]), -5, -5)=='M'){
      mat = as.numeric(gsub("[^0-9.-]", "", colnames(country_df2[,j])))/12 
    } else {
      mat = as.numeric(gsub("[^0-9.-]", "", colnames(country_df2[,j])))
    }
    if (j==1){
      maturity<-mat
    }else{
      maturity<-cbind(maturity,mat)
    }}
  
  

  country_lambdas<-as.data.frame(t(apply(country_df2,1, #apply the lambda function for the whole timeseries
                                       lambda_function,data_frame=country_df2)))


  colnames(country_lambdas)<-c(paste0(currency_mapping[i,1],"_L_1"), #rename columns 
                               paste0(currency_mapping[i,1],"_L_2"),
                               paste0(currency_mapping[i,1],"_L_3"))

  if (i==1) #create a global lambda frame
    {
      global_lambdas<-country_lambdas
    } else {
          global_lambdas<-cbind(global_lambdas, country_lambdas)
            }
  
  }

global_lambdas1<-global_lambdas

column_reorder<-function(df)
  {
  col_names_list = strsplit(names(df),"_") 
  col_names_df = data.frame(matrix(unlist(col_names_list),nrow=length(names(df)),byrow=T))
  col_names_df = col_names_df[order(col_names_df$X3,col_names_df$X1, decreasing = F),]
  col_names_df_reformatted = paste0(col_names_df$X1,"_",col_names_df$X2,"_",col_names_df$X3)
  df = df[,col_names_df_reformatted]
  return(df)
}

global_lambdas<-column_reorder(global_lambdas)

write.csv(global_lambdas,"global_lambdas_full.csv", row.names = TRUE) #save the results to a csv





