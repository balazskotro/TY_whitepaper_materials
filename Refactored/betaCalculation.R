#This is the NS Equation by Diebold and Li (2006)
nelsonSiegelCalculate<-function(theta,lambda,beta0,beta1,beta2)
  {
  beta0 + beta1*(1-exp(-lambda * theta))/(lambda * theta) + 
    beta2*((1-exp(-lambda * theta))/(lambda * theta) - exp(-lambda * theta))
  }
#---

#This is a minimizer function which minimizes the errors between estimated and observed yield
nelsonSiegelMinimizer <- function(data, param)
  {
  with(data, sum((yield - nelsonSiegelCalculate(maturity, 0.609, param[1], param[2], param[3]))^2))
  }
#---
curveRatesToBetas<-function(usage, curveRatesPivot){
  
if (usage=="excel") {
  globalBetas<-read.csv(file = paste0(workingDirectory, "\\parsedValues\\globalBetas.csv")) 
  rownames(globalBetas)<-globalBetas[,1]
  globalBetas<-globalBetas[,-1]
  currency<-as.data.frame(unique(substr(names(curveRatesPivot[-1]),1,3)))
} else {

currency<-as.data.frame(unique(substr(names(curveRatesPivot[-1]),1,3)))

for (i in 1:nrow(currency)){ #iterate for all countries{ 
  
  print("Calculating betas...")
  print(paste0(i," / ",nrow(currency)," -- ",currency[i,1]))
  
  #select the yield series of current country
  countryDataFrame<-curveRatesPivot%>% 
    dplyr::select(Date, starts_with(currency[i,1]))
  countryDataFrame <- xts(countryDataFrame[,-1], order.by=as.Date(countryDataFrame[,1])) #create an xts object
  
  for (j in 1: ncol(countryDataFrame)){ 
    
    if (str_sub(colnames(countryDataFrame[,j]), -5, -5)=='M'){
      mat = as.numeric(gsub("[^0-9.-]", "", colnames(countryDataFrame[,j])))/12 
    } else {
      mat = as.numeric(gsub("[^0-9.-]", "", colnames(countryDataFrame[,j])))
    }
    if (j==1){
      maturity<-mat
    }else{
      maturity<-cbind(maturity,mat)}}

  lambda<-0.0609
  
  for (k in 1: nrow(countryDataFrame)){ 
    
    Xmaturity<-data.frame(1,(1-(exp(t(-maturity)*12*lambda)))/
                        ((t(maturity)*12*lambda)),(1-(exp(t(-maturity)*12*lambda)))/
                        ((t(maturity)*12*lambda))-exp(t(-maturity)*12*lambda))
    Xmaturity<-as.matrix(Xmaturity)
    Ymaturity<-as.matrix(t(countryDataFrame[k,]))
    
    betas<-inv(t(Xmaturity)%*%Xmaturity)%*%t(Xmaturity)%*%Ymaturity
    betas<-as.data.frame(t(betas))
    
    colnames(betas)<-c(paste0(currency[i,1],"_B_1"), #rename columns 
                          paste0(currency[i,1],"_B_2"),
                          paste0(currency[i,1],"_B_3"))

    if (k==1) {
      countryBetas<-betas
    } else {
      countryBetas<-rbind(countryBetas, betas)}
  }
  
  if (i==1){
    globalBetas<-countryBetas} 
  else {globalBetas<-cbind(globalBetas, countryBetas)}
  
}

#global_lambdas1<-global_lambdas

columnReorder<-function(df)
{
  columnNameList = strsplit(names(df),"_") 
  columnNames = data.frame(matrix(unlist(columnNameList),nrow=length(names(df)),byrow=T))
  columnNames = columnNames[order(columnNames$X3,columnNames$X1, decreasing = F),]
  columnNames = paste0(columnNames$X1,"_",columnNames$X2,"_",columnNames$X3)
  df = df[,columnNames]
  return(df)
}

globalBetas<-columnReorder(globalBetas)

globalBetasStatistics<-stat.desc(globalBetas)

globalBetasStatistics<-rbind(globalBetasStatistics,NA)
rownames(globalBetasStatistics)[nrow(globalBetasStatistics)]<-"JB_value"

jarqueBeraList<-apply(globalBetas, 2,jarque.bera.test)

for (k in 1: length(jarqueBeraList)) {
  globalBetasStatistics[nrow(globalBetas),k]<-jarqueBeraList[[k]][["statistic"]][["X-squared"]]
}

globalBetasStatistics<-rbind(globalBetasStatistics,NA)
rownames(globalBetasStatistics)[nrow(globalBetasStatistics)]<-"JB_p"

for (k in 1: length(jarqueBeraList)) {
  globalBetasStatistics[nrow(globalBetasStatistics),k]<-jarqueBeraList[[k]][["p.value"]]
}


write.csv(globalBetas,paste0(workingDirectory, "\\parsedValues\\globalBetas.csv"), row.names = TRUE) #save the results to a csv
write.csv(globalBetasStatistics, paste0(workingDirectory, "\\parsedValues\\globalBetasStatistics.csv"), 
          row.names = TRUE) #save the results to a csv

}
  globalBetaList <- list(globalBetas, currency)
 return (globalBetaList) 
  }


globalBetas <- curveRatesToBetas(usage, curveRatesPivot)[[1]]
currency <- curveRatesToBetas(usage, curveRatesPivot)[[2]]

