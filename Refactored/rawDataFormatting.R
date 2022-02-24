rawColumnReorder<-function(df) {
  firstKey<-substr(names(df[-1]),1,3)
  secodKey<-regmatches(names(df[-1]), gregexpr("[[:digit:]]+", names(df[-1])))
  secondKey<-(as.numeric(as.character((unlist(secodKey)))))
  thirdKey<-str_sub(names(df[-1]),-5,)
  ordering<-cbind(firstKey,secondKey,thirdKey)
  ordering <- ordering[order(ordering[,1],ordering[,3], as.numeric(ordering[,2]), decreasing = F),]
  coulumNamesOrdered=paste0(ordering[,1],ordering[,2],ordering[,3])
  df = df[,coulumNamesOrdered]
}

curveRates<-function(frequency) {

curveRates<-read_excel("C:\\Research\\Sovereign_interconnectedness\\Data\\Yield_curve_rates.xlsx", sheet="SUM")
curveRatesPivot<-rawColumnReorder(curveRates)
curveRatesPivot<-sapply(curveRatesPivot, function(x) ifelse(x == "NULL", NA, x))
curveRatesPivot<-as.data.frame(curveRatesPivot)
curveRatesPivot<-cbind(curveRates[,1],curveRatesPivot)
curveRatesPivot <- curveRatesPivot %>% 
  mutate(Date = as.Date(Date,format = "%Y-%m-%d")) %>%
  arrange(Date)

#This is just a check for the vectors being 80+% non empty
#Commented out as does not change the input matrix

#curveRatesPivot<-curveRatesPivot[, which(colMeans(!is.na(curveRatesPivot)) > 0.8)]

#clean NA-s by forward filling T-1 rates
curveRatesPivot<-curveRatesPivot%>% 
  do(na.locf(.))

curveRatesPivot<-na.locf(na.locf(curveRatesPivot), fromLast = TRUE)

#Check for empty cells
#Commented out as they are 0

#sum(is.na(curveRatesPivot))

#curve_universe_pivot<-as.data.frame(curve_universe_pivot)
#stat.desc(curve_universe_pivot)

#This is just a check for weekdays
#Strangly R studio uses Hungarian names for days
#Commented out as does not change the input matrix

#curveRatesPivot<-curveRatesPivot[!(weekdays(as.Date(curveRatesPivot$Date)) %in% c('Szombat','vasárnap')),]

#Tailor data according to desired frequency
if (frequency == "daily") {
  
}else if (frequency == "weekly"){
  curveRatesPivot <-curveRatesPivot[
    weekdays(as.Date(curveRatesPivot$Date)) == 'péntek',]
  
} else if (frequency == "monthly"){
  curveRatesPivot <- curveRatesPivot[curveRatesPivot$Date %in% 
                     as.data.frame(as.Date(tapply(
                     curveRatesPivot$Date, substr(
                     curveRatesPivot$Date, 1, 7), max)))[, 1],]
}

curveRatesStatistics<-stat.desc(curveRatesPivot)
write.csv(curveRatesStatistics,paste0(workingDirectory, "\\parsedValues\\curveRatesStatistics.csv"), row.names = TRUE)

return(curveRatesPivot)
}

curveRatesPivot <- curveRates(frequency)
