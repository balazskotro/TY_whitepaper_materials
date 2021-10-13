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




if (usage=="excel") {
 global_lambdas1<-read.csv(file = 'C:\\Research\\Sovereign_interconnectedness\\Data\\global_lambdas_full_lin.csv') 
 global_lambdas<-global_lambdas1[,-1]
 rownames(global_lambdas)<-global_lambdas1[,1]
 currency_mapping<-as.data.frame(unique(substr(names(curve_universe_pivot[-1]),1,3)))
 } else {

#--------------------------------------------------------------------------------------------------
currency_mapping<-as.data.frame(unique(substr(names(curve_universe_pivot[-1]),1,3)))
for (i in 1:nrow(currency_mapping)) #iterate for all countries
{ 
  
  print("Calculating lambdas...")
  print(paste0(i," / ",nrow(currency_mapping)," -- ",currency_mapping[i,1]))
country_df<-curve_universe_pivot%>% #select the yield series of current country
  dplyr::select(Date, starts_with(currency_mapping[i,1]))




#country_df2<-myFun(country_df) 
country_df2<-country_df
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

time=max(maturity)*3
var=seq(0,1, by=0.0001)
lv<-(1-exp(-time*var))/(time*var)-exp(-time*var)
lambda<-var[match(max(lv,na.rm = TRUE),lv)]
lambda<-0.0609
print(paste("lambda for ", currency_mapping[i,1], "= ",lambda ))

for (k in 1: nrow(country_df2)){ 

X_mat<-data.frame(1,(1-(exp(t(-maturity)*12*lambda)))/((t(maturity)*12*lambda)),(1-(exp(t(-maturity)*12*lambda)))/((t(maturity)*12*lambda))-exp(t(-maturity)*12*lambda))
X_mat<-as.matrix(X_mat)
Y_mat<-as.matrix(t(country_df2[k,]))

Betas<-inv(t(X_mat)%*%X_mat)%*%t(X_mat)%*%Y_mat
Betas_df<-as.data.frame(t(Betas))

colnames(Betas_df)<-c(paste0(currency_mapping[i,1],"_B_1"), #rename columns 
                             paste0(currency_mapping[i,1],"_B_2"),
                             paste0(currency_mapping[i,1],"_B_3"))


if (k==1) #create a global lambda frame
{
  country_lambdas<-Betas_df
} else {
  country_lambdas<-rbind(country_lambdas, Betas_df)
}

}
if (i==1) #create a global lambda frame
{
  global_lambdas<-country_lambdas
} else {
  global_lambdas<-cbind(global_lambdas, country_lambdas)
}

}

#global_lambdas1<-global_lambdas

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

global_lambdas_stat<-stat.desc(global_lambdas)

global_lambdas_stat<-rbind(global_lambdas_stat,NA)
rownames(global_lambdas_stat)[nrow(global_lambdas_stat)]<-"JB_value"

jb_list<-apply(global_lambdas, 2,jarque.bera.test)



for (k in 1: length(jb_list)) {
global_lambdas_stat[nrow(global_lambdas_stat),k]<-jb_list[[k]][["statistic"]][["X-squared"]]
}

global_lambdas_stat<-rbind(global_lambdas_stat,NA)
rownames(global_lambdas_stat)[nrow(global_lambdas_stat)]<-"JB_p"

for (k in 1: length(jb_list)) {
  global_lambdas_stat[nrow(global_lambdas_stat),k]<-jb_list[[k]][["p.value"]]
}


write.csv(global_lambdas,"C:\\Research\\Sovereign_interconnectedness\\Data\\global_lambdas_full_lin.csv", row.names = TRUE) #save the results to a csv
write.csv(global_lambdas_stat,"C:\\Research\\Sovereign_interconnectedness\\Data\\global_lambdas_stat.csv", row.names = TRUE) #save the results to a csv
}
