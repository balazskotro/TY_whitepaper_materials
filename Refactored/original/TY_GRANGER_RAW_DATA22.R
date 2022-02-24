raw_column_reorder<-function(df) {
  first_key<-substr(names(df[-1]),1,2)
  secod_key<-regmatches(names(df[-1]), gregexpr("[[:digit:]]+", names(df[-1])))
  second_key<-(as.numeric(as.character((unlist(secod_key)))))
  third_key<-str_sub(names(df[-1]),-5,)
  ordering<-cbind(first_key,second_key,third_key)
  ordering = ordering[order(ordering[,1],ordering[,3], as.numeric(ordering[,2]), decreasing = F),]
  col_names_df_reformatted=paste0(ordering[,1],ordering[,2],ordering[,3])
  df = df[,col_names_df_reformatted]
}

datelist<-read.csv("C:\\Users\\x\\Desktop\\stat\\datelist.csv")
  
curve_query<-read.csv(file = 'C:\\Users\\x\\Desktop\\factor_model_res\\sov_test.csv')

curve_query[3]<-sapply(curve_query[3], substring, 1, 10)


curve_universe_df<-as_tibble(curve_query) #convert to tibble and get rid of mds_uid

curve_universe_df<-datelist %>% left_join(curve_universe_df)


curve_universe_pivot<-curve_universe_df %>%   #create pivot table as the node_idents as columns
  pivot_wider(names_from = Instrument,values_from = Bid.Yield)





#curve_universe_pivot_ordered<-raw_column_reorder(curve_universe_pivot)

#curve_universe_pivot_ordered<-as.data.frame(curve_universe_pivot_ordered)


#curve_universe_pivot_ordered<-sapply(curve_universe_pivot_ordered, function(x) ifelse(x == "NULL", NA, x))

curve_universe_pivot_ordered<-as.data.frame(curve_universe_pivot_ordered)

#curve_universe_pivot<-cbind(curve_universe_pivot[,1],curve_universe_pivot_ordered)




curve_universe_pivot <- curve_universe_pivot %>% #order the rows by time
  mutate(Date = as.Date(Date,format = "%Y-%m-%d")) %>%
  arrange(Date)





curve_universe_pivot<-curve_universe_pivot[, which(colMeans(!is.na(curve_universe_pivot)) > 0.8)]


# curve_universe_pivot<-curve_universe_pivot%>% #clean NA-s by forward filling T-1 rates
#   do(na.locf(.))

curve_universe_pivot<-na.locf(curve_universe_pivot, fromLast = TRUE)

sum(is.na(curve_universe_pivot))


curve_universe_pivot<-as.data.frame(curve_universe_pivot)



curve_universe_pivot['9']<-(curve_universe_pivot['8']+curve_universe_pivot['10'])/2
curve_universe_pivot['12']<-curve_universe_pivot['10']*3/5+curve_universe_pivot['15']*2/5
curve_universe_pivot['15']<-curve_universe_pivot['12']



names(curve_universe_pivot)


names(curve_universe_pivot)<-c("Date", "1M","3M","6M","9M","1","2","3","4","5","6","7","8","9","10","15","20","30","40")

curve_universe_pivot<-sqldf("select Date, `1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`15` from curve_universe_pivot")

curve_universe_pivot<-sqldf("select Date, `1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`12`,`15` from curve_universe_pivot")


melted<-reshape2::melt(curve_universe_pivot, id = c("Date"))
melted$UID<-'JPY_sovr'
melted<-sqldf("select Date, UID, variable tenor, value yield from melted")

write.csv(curve_query,paste("C:\\Users\\x\\Desktop\\stat\\sovereign.csv"), row.names = FALSE, quote=FALSE)
