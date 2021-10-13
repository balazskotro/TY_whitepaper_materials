rm(list = ls()) #delete everything

setwd("C:\\Users\\x\\Desktop\\stat\\logs") #Rewrite this to a proper working direction

#rm(list = ls()) #delete everything
  
curve_query_alt<-read.csv(file = 'C:\\Users\\x\\Desktop\\factor_model_res\\yield_to_load.csv')
#curve_query_alt<-read.csv(file = 'C:\\Users\\x\\Desktop\\artificialset.csv')
#datelist<-sqldf("select distinct date from curve_query_alt")
  
  
#curve_query[3]<-sapply(curve_query[3], substring, 1, 10)

curve_universe_df_alt<-as_tibble(curve_query_alt) #convert to tibble and get rid of mds_uid
curve_universe_df_alt[3]<-readr::parse_number(curve_universe_df_alt$variable)
names(curve_universe_df_alt)[3]<-"tenor"

curve_universe_df_alt<-sqldf("select * from curve_universe_df_alt where tenor not in (20,25,30)")
curve_universe_df_alt<-sqldf("select * from curve_universe_df_alt where Date>'2014-12-31'")
datelist<-sqldf("select distinct date from curve_universe_df_alt")
uid_list<-sqldf("select distinct uid from curve_universe_df_alt")

uid_list_p<-read.csv(file = 'C:\\Users\\x\\Desktop\\problematic_curves.csv')

for (i in 1: nrow(uid_list_p)) {
 
curve_universe_df_alt_unique<-dplyr::filter(curve_universe_df_alt, grepl( uid_list_p[i,1], UID))

print (paste("run num: ",i," curve: ", uid_list[i,1]))

#curve_universe_df_alt_unique<-("select distinct * from curve_universe_df_alt_unique")

curve_universe_pivot_alt<-curve_universe_df_alt_unique %>%   #create pivot table as the node_idents as columns
  pivot_wider(names_from = tenor,values_from = value)

curve_universe_pivot2<-as.data.frame(curve_universe_pivot_alt)
curve_universe_pivot_alt<-datelist %>% left_join(curve_universe_pivot2)


#curve_universe_pivot_ordered<-raw_column_reorder(curve_universe_pivot)
#curve_universe_pivot_alt<-as.data.frame(sapply(curve_universe_pivot_alt, function(x) ifelse(x == "NULL", NA, x)))
#curve_universe_pivot_alt<-as.data.frame(curve_universe_pivot_alt)
#curve_universe_pivot_alt<-cbind(curve_universe_pivot[,1],curve_universe_pivot_ordered)

# curve_universe_pivot_alt <- curve_universe_pivot_alt %>% #order the rows by time
#   mutate(Date = as.Date(Date,format = "%Y-%m-%d")) %>%
#   arrange(Date)


curve_universe_pivot_alt <-curve_universe_pivot_alt%>%
  mutate(Date = as.Date(Date,format = "%Y-%m-%d"))%>%
  arrange(Date)


curve_universe_pivot_altf<-as.data.frame(curve_universe_pivot_alt[, which(colMeans(!is.na(curve_universe_pivot_alt)) > 0.8)])
curve<-curve_universe_pivot_altf

if (ncol(curve_universe_pivot_altf)<8){
fileConn<-file(paste( uid_list_p[i,1],".txt"))
writeLines(paste( uid_list_p[i,1], " Curve has less than 6 initial tenors"), fileConn)
close(fileConn)
next
}


year1<-NULL
delayedAssign("do.next", {next})
if ("1" %in% names(curve_universe_pivot_altf)==TRUE)
  {print('Tenor 1 is okay')} else{
    tryCatch(year1<<-curve_universe_pivot_altf['2'],
    print('Tenor 1 is calculated'),error = function(e) 
    {fileConn<-file(paste( uid_list_p[i,1],".txt"))
    writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 1Y nor 2Y tenors"), fileConn)
    close(fileConn)
    print("Exiting here!")
    force(do.next)})}

if (!is.null(year1)){
  curve<-cbind(curve,year1)
  names(curve)[length(names(curve))]<-"1" 
}

year2<-NULL
delayedAssign("do.next", {next})
if ("2" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 2 is okay')} else{
  tryCatch(year2<<-(curve_universe_pivot_altf['1']+curve_universe_pivot_altf['3'])/2,
  print('Tenor 2 is calculated'),error = function(e)
  {fileConn<-file(paste( uid_list_p[i,1],".txt"))
  writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 2Y nor 3Y tenors"), fileConn)
  close(fileConn)
  print("Exiting here!")
  force(do.next)})}

if (!is.null(year2)){
  curve<-cbind(curve,year2)
  names(curve)[length(names(curve))]<-"2" 
}

year3<-NULL
delayedAssign("do.next", {next})
if ("3" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 3 is okay')} else{
  tryCatch(year3<<-(curve_universe_pivot_altf['2']+curve_universe_pivot_altf['4'])/2,
  print('Tenor 3 is calculated'),error = function(e)
  {fileConn<-file(paste( uid_list_p[i,1],".txt"))
  writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 3Y nor 4Y tenors"), fileConn)
  close(fileConn)
  print("Exiting here!")
  force(do.next)})}

if (!is.null(year3)){
  curve<-cbind(curve,year3)
  names(curve)[length(names(curve))]<-"3" 
}


year4<-NULL
delayedAssign("do.next", {next})
if ("4" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 4 is okay')} else{
  tryCatch(year4<<-(curve_universe_pivot_altf['3']+curve_universe_pivot_altf['5'])/2,
  print('Tenor 4 is calculated'),error = function(e)
  {fileConn<-file(paste( uid_list_p[i,1],".txt"))
  writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 4Y nor 5Y tenors"), fileConn)
  close(fileConn)
  print("Exiting here!")
  force(do.next)})}

if (!is.null(year4)){
  curve<-cbind(curve,year4)
  names(curve)[length(names(curve))]<-"4" 
}

year5<-NULL
delayedAssign("do.next", {next})
if ("5" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 5 is okay')} else{
  tryCatch(year5<<-(curve_universe_pivot_altf['4']+curve_universe_pivot_altf['6'])/2,
       print('Tenor 5 is calculated'),error = function(e)
  {fileConn<-file(paste( uid_list_p[i,1],".txt"))
  writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 5Y nor 6Y tenors"), fileConn)
  close(fileConn)
  print("Exiting here!")
  force(do.next)
  })}

if (!is.null(year5)){
  curve<-cbind(curve,year5)
  names(curve)[length(names(curve))]<-"5" 
}


year6<-NULL
delayedAssign("do.next", {next})
  if ("6" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 6 is okay')} else{
  tryCatch(year6<<-(curve_universe_pivot_altf['5']+curve_universe_pivot_altf['7'])/2,
           print("Tenor 6 is calculated"),error = function(e)
    {fileConn<-file(paste( uid_list_p[i,1],".txt"))
    writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 6Y nor 7Y tenors"), fileConn)
    close(fileConn)
    print("Exiting here!")
    force(do.next)})}

if (!is.null(year6)){
  curve<-cbind(curve,year6)
  names(curve)[length(names(curve))]<-"6" 
}


year7<-NULL
delayedAssign("do.next", {next})
if ("7" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 7 is okay')} else{
  tryCatch(year7<<-(curve_universe_pivot_altf['6']+curve_universe_pivot_altf['8'])/2,
           print("Tenor 7 is calculated"),error = function(e)
           {fileConn<-file(paste( uid_list_p[i,1],".txt"))
           writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 7Y nor 8Y tenors"), fileConn)
           close(fileConn)
           print("Exiting here!")
           force(do.next)
           })}

if (!is.null(year7)){
  curve<-cbind(curve,year7)
  names(curve)[length(names(curve))]<-"7" 
}

year8<-NULL
delayedAssign("do.next", {next})
if ("8" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 8 is okay')
  } else{
  tryCatch(year8<<-(curve_universe_pivot_altf['7']+curve_universe_pivot_altf['9'])/2,
           print("Tenor 8 is calculated"),
           error = function(e)
           {fileConn<-file(paste( uid_list_p[i,1],".txt"))
           writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 8Y nor 9Y tenors"), fileConn)
           close(fileConn)
           print("Exiting here!")
           #force(do.next)
           })}

if (!is.null(year8)){
curve<-cbind(curve,year8)
names(curve)[length(names(curve))]<-"8" 
}


year9<-NULL
delayedAssign("do.next", {next})
if ("9" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 9 is okay')} else{
  tryCatch(year9<<-(curve_universe_pivot_altf['8']+curve_universe_pivot_altf['10'])/2,
                      print("Tenor 9 is calculated"),
           error = function(e)
           {fileConn<-file(paste( uid_list_p[i,1],".txt"))
           writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 9Y nor 10Y tenors"), fileConn)
           close(fileConn)
           print("Exiting here!")
                      force(do.next)})}



if (!is.null(year9)){
  curve<-cbind(curve,year9)
  names(curve)[length(names(curve))]<-"9" 
}

year10<-NULL
delayedAssign("do.next", {next})
if ("10" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 10 is okay')} else{
  tryCatch(year10<<-(curve_universe_pivot_altf['9']*(2/3)+curve_universe_pivot_altf['12']*(1/3)),
           print("Tenor 10 is calculated"),error = function(e)
           {fileConn<-file(paste( uid_list_p[i,1],".txt"))
           writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 10Y nor 12Y tenors"), fileConn)
           close(fileConn)
           print("Exiting here!")
           force(do.next)})}

if (!is.null(year10)){
  curve<-cbind(curve,year10)
  names(curve)[length(names(curve))]<-"10" 
}

year12<-NULL
delayedAssign("do.next", {next})
if ("12" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 12 is okay')} else{
  tryCatch(year12<<-(curve_universe_pivot_altf['10']*(3/5)+curve_universe_pivot_altf['15']*(2/5)),
           print("Tenor 12 is calculated"),error = function(e)
           {fileConn<-file(paste( uid_list_p[i,1],".txt"))
           writeLines(paste( uid_list_p[i,1], " Curve is discarded because it has neither 12Y nor 15Y tenors"), fileConn)
           close(fileConn)
           print("Exiting here!")
           force(do.next)})}

if (!is.null(year12)){
  curve<-cbind(curve,year12)
  names(curve)[length(names(curve))]<-"12" 
}

year15<-NULL
delayedAssign("do.next", {next})
if ("15" %in% names(curve_universe_pivot_altf)==TRUE)
{print('Tenor 15 is okay')} else{
  tryCatch(year15<<-curve_universe_pivot_altf['12'],
           print("Tenor 15 is calculated"),error = function(e)
           {fileConn<-file(paste( uid_list_p[i,1],".txt"))
           writeLines("Curve is discarded because it has neither 12Y nor 15Y tenors", fileConn)
           close(fileConn)
           print("Exiting here!")
           force(do.next)})}

if (!is.null(year15)){
  curve<-cbind(curve,year15)
  names(curve)[length(names(curve))]<-"15" 
}


#curve<-curve%>% #clean NA-s by forward filling T-1 rates
#  do(na.locf(.))


#curve<-curve_universe_pivot_altf

curve<-na.locf(curve, fromLast = TRUE)

sum(is.na(curve))

melted<-reshape2::melt(curve, id = c("Date","UID"))

melted$variable<-as.numeric(melted$variable)

write.csv(melted,paste("C:\\Users\\x\\Desktop\\stat\\results\\",uid_list_p[i,1],".csv"), row.names = FALSE, quote=FALSE)


fileConn<-file(paste(uid_list_p[i,1],".txt"))
writeLines(paste( uid_list_p[i,1], " Curve is derived successfully"), fileConn)
close(fileConn)


}




# if (ncol(curve_universe_pivot_altf)<13){
#
#
#   fileConn<-file("output.txt")
#   writeLines(paste0("'0#AVBUSDABMK="," ", "is discarded due length. ", ncol(curve_universe_pivot_alt)-ncol(curve_universe_pivot_altf), " cols got discarded"), fileConn)
#   close(fileConn)
#
# }


# curve_universe_pivot_altf<-curve_universe_pivot_alt%>% #clean NA-s by forward filling T-1 rates
#   do(na.locf(.))
# 
# curve_universe_pivot_altf<-na.locf(na.locf(curve_universe_pivot_alt), fromLast = TRUE)
# 
# sum(is.na(curve_universe_pivot_altf))


#curve_universe_pivot2<-as.data.frame(curve_universe_pivot_alt)
