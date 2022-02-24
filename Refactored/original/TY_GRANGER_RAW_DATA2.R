raw_column_reorder<-function(df) {
  first_key<-substr(names(df[-1]),1,3)
  secod_key<-regmatches(names(df[-1]), gregexpr("[[:digit:]]+", names(df[-1])))
  second_key<-(as.numeric(as.character((unlist(secod_key)))))
  third_key<-str_sub(names(df[-1]),-5,)
  ordering<-cbind(first_key,second_key,third_key)
  ordering = ordering[order(ordering[,1],ordering[,3], as.numeric(ordering[,2]), decreasing = F),]
  col_names_df_reformatted=paste0(ordering[,1],ordering[,2],ordering[,3])
  df = df[,col_names_df_reformatted]
}

my_data<-read_excel("C:\\Research\\Sovereign_interconnectedness\\Data\\Yield_curve_rates.xlsx", sheet="SUM")
uspos<-read_excel("C:\\Research\\Sovereign_interconnectedness\\Data\\US_positive_vector.xlsx", sheet="SUM")
uspos <- uspos %>% #order the rows by time
  mutate(Date = as.Date(Date,format = "%Y-%m-%d")) %>%
  arrange(Date)
  
# curve_query<-read.csv(file = 'C:\\Users\\x\\Desktop\\factor_model_res\\sovr_test.csv')
# 
# curve_query[3]<-sapply(curve_query[3], substring, 1, 10)
# 
# curve_universe_df<-as_tibble(curve_query) #convert to tibble and get rid of mds_uid
# 
# curve_universe_pivot<-curve_universe_df %>%   #create pivot table as the node_idents as columns
#   pivot_wider(names_from = Instrument,values_from = Bid.Yield)

curve_universe_pivot<-my_data

curve_universe_pivot_ordered<-raw_column_reorder(curve_universe_pivot)


curve_universe_pivot_ordered<-sapply(curve_universe_pivot_ordered, function(x) ifelse(x == "NULL", NA, x))

curve_universe_pivot_ordered<-as.data.frame(curve_universe_pivot_ordered)

curve_universe_pivot<-cbind(curve_universe_pivot[,1],curve_universe_pivot_ordered)


curve_universe_pivot <- curve_universe_pivot %>% #order the rows by time
  mutate(Date = as.Date(Date,format = "%Y-%m-%d")) %>%
  arrange(Date)

curve_universe_pivot<-curve_universe_pivot[, which(colMeans(!is.na(curve_universe_pivot)) > 0.8)]


curve_universe_pivot<-curve_universe_pivot%>% #clean NA-s by forward filling T-1 rates
  do(na.locf(.))

curve_universe_pivot<-na.locf(na.locf(curve_universe_pivot), fromLast = TRUE)

sum(is.na(curve_universe_pivot))


curve_universe_pivot<-as.data.frame(curve_universe_pivot)
#stat.desc(curve_universe_pivot)

curve_universe_pivot<-curve_universe_pivot[!(weekdays(as.Date(curve_universe_pivot$Date)) %in% c('Szombat','vasárnap')),]


curve_universe_stat<-stat.desc(curve_universe_pivot)
#acf_list<-apply(curve_universe_pivot, 2,acf)

#sapply(acf, FUN = function (u) acf(lag=10) )
#f<-apply(curve_universe_pivot_ordered, 2, function(ts) acf(ts, lag.max=10))

#write.csv(curve_universe_stat,"C:\\Research\\Sovereign_interconnectedness\\Data\\curve_universe_stat.csv", 
 #         row.names = TRUE) #save the results to a csv


