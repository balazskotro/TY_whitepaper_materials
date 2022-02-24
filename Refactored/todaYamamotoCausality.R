source("stationarityFunctions.R")

maxDifferenceOrder<-timeSeriesDifferentiator(globalBetas, typeOfUnitRootTest)

computeConnectionMatrix<-function(globalBetas,lag, varType,typeOfInformationCriterion){

firstVarResult<-VARselect(globalBetas, lag, type = varType)

if(typeOfInformationCriterion=="AIC")
{
  varOrder<- firstVarResult$selection[1] 
}else if (typeOfInformationCriterion=="HQ")
{
  varOrder<- firstVarResult$selection[2]
}else if (typeOfInformationCriterion=="SC")
{
  varOrder<- firstVarResult$selection[3]
}else if (typeOfInformationCriterion=="FPE")
{
  varOrder<- firstVarResult$selection[4]
}else
{
  print("Incorrect information Criterion!")
}

secondVarResult<-VAR(globalBetas, p=varOrder, type = varType)
if (1/roots(secondVarResult)[[2]]>1){
cat(green("Stability analysis passes!\n"))
} else{
  cat(yellow("Warning: Stability analysis did not pass!\n"))  
}

thirdVarResult<-VAR(globalBetas,p=(varOrder+maxDifferenceOrder), type=varType)
#var_results_1$varresult
#summary(var_results_1)


connectednessMatrix<-as.data.frame(matrix(data= NA, 
                                    nrow = ncol(globalBetas), 
                                    ncol = ncol(globalBetas)))

for (i in 1:nrow(connectednessMatrix)) 
{
  for (j in 1:ncol(connectednessMatrix)) 
  { 
    causalityMeasure<-wald.test(b=coef(thirdVarResult$varresult[[i]]),
                     Sigma = vcov(thirdVarResult$varresult[[i]]),
                     Terms = c(j,j+(1:varOrder)*nrow(currency)*3))
    

    
    connectednessMatrix[j,i]<-causalityMeasure$result$chi2[3] 
    
    }}

colnames(connectednessMatrix)<-colnames(globalBetas)
rownames(connectednessMatrix)<-colnames(globalBetas)

return(connectednessMatrix)}

edgeCounterBySubnetwork<-function(edgeCounts,betaGroups,subNetID, firstExcludedSubNetID, secondExcludedSubNetID){
  
  subNetwork<-edgeCounts[unlist(betaGroups[subNetID]),unlist(betaGroups[subNetID])]
  edgesSumWithin<-sum(edgeCounts[unlist(betaGroups[subNetID]),unlist(betaGroups[subNetID])])
  
  
  incomingEdges<-as.data.frame(apply(subNetwork, 1,sum))
  colnames(incomingEdges) <- 'numberOfEdges'
  incomingEdges<-incomingEdges[order(-incomingEdges$numberOfEdges),,drop=FALSE]
  colnames(incomingEdges) <- 'numberOfEdges'
  
  exSubNetwork<-(edgeCounts[unlist(betaGroups[subNetID]),unlist(betaGroups[firstExcludedSubNetID])])+
    (edgeCounts[unlist(betaGroups[subNetID]),unlist(betaGroups[secondExcludedSubNetID])])
  
  outgoingEdges<-as.data.frame(apply(exSubNetwork, 1,sum))
  colnames(outgoingEdges) <- 'numberOfEdges'
  outgoingEdges<-outgoingEdges[order(-outgoingEdges$numberOfEdges),,drop=FALSE]
  colnames(outgoingEdges) <- 'numberOfEdges'
  
  resultList <- list(incomingEdges, outgoingEdges)
  
  return (resultList)
}



connectednessMatrix <- computeConnectionMatrix(globalBetas,7, 'both',typeOfInformationCriterion)


edgeCounts  <- as.matrix(connectednessMatrix)
diag(edgeCounts)<-NA
edgeCounts <- ifelse(abs(edgeCounts) <= ((grangerPValue)), 1, 0)
diag(edgeCounts)<-0

betaGroups   = list(1:((ncol(edgeCounts))/3), 
                 ((ncol(edgeCounts))/3+1):(((ncol(edgeCounts))/3)*2), 
                 ((((ncol(edgeCounts))/3)*2)+1):(ncol(edgeCounts)))


allEdges<-sum(edgeCounts)
outgoingEdgesByNodes<-as.data.frame(apply(edgeCounts, 1,sum))
incomingEdgesByNodes<-as.data.frame(apply(edgeCounts, 2,sum))

edgeCountsByNodes<-outgoingEdgesByNodes+incomingEdgesByNodes

rownames(outgoingEdgesByNodes)<-paste0(rownames(outgoingEdgesByNodes),"_outgoing")
rownames(incomingEdgesByNodes)<-paste0(rownames(incomingEdgesByNodes),"_incoming")
rownames(edgeCountsByNodes)<-paste0(rownames(edgeCountsByNodes),"_sum")

allPotentialEdgesWithinSubnetwork<-factorial(nrow(edgeCounts[unlist(betaGroups[1]),unlist(betaGroups[1])]))/
  (factorial(2)*factorial(nrow(edgeCounts[unlist(betaGroups[1]),unlist(betaGroups[1])])-2)) #66

allPotentialEdgesOuter<-factorial(nrow(edgeCounts))/
  (factorial(2)*factorial(nrow(edgeCounts)-2))-3*
  (factorial(nrow(edgeCounts[unlist(betaGroups[1]),unlist(betaGroups[1])]))/
  (factorial(2)*factorial(nrow(edgeCounts[unlist(betaGroups[1]),unlist(betaGroups[1])])-2))) #432

incomingEdgesLevel<-edgeCounterBySubnetwork(edgeCounts,betaGroups,1, 2, 3)[[1]]
outgoingEdgesLevel<-edgeCounterBySubnetwork(edgeCounts,betaGroups,1, 2, 3)[[2]]

incomingEdgesSlope<-edgeCounterBySubnetwork(edgeCounts,betaGroups,2, 1, 3)[[1]]
outgoingEdgesSlope<-edgeCounterBySubnetwork(edgeCounts,betaGroups,2, 1, 3)[[2]]

incomingEdgesCurvature<-edgeCounterBySubnetwork(edgeCounts,betaGroups,3, 1, 2)[[1]]
outgoingEdgesCurvature<-edgeCounterBySubnetwork(edgeCounts,betaGroups,3, 1, 2)[[2]]

