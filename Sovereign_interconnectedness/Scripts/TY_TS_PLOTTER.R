library("readxl")
library("reshape2")
library("lubridate")
library("ggplot2")
library(scales)

input_data_750 <- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\edge_counts.xlsx", 
                      sheet = "Full_percentage_750")

melted_750<-melt(input_data_750,  id.vars = "Date", value.name = "value")
melted_750$Date<-ymd(melted_750$Date)



sovereign <- data.frame(xmin=as.Date(c("2008-03-20")),
                      xmax=as.Date(c("2009-12-31")))

euro <- data.frame(xmin=as.Date(c("2011-04-07")),
                      xmax=as.Date(c("2012-07-26")))

ggplot() +
  geom_rect(data=sovereign,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="pink", alpha=0.5) +
  geom_rect(data=euro,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Dynamic Toda-Yamamoto Spillover Analysis \n Window size: 750; Lag: based on AIC")+
  geom_line(data=melted_750, aes(Date, value, colour = variable, size = variable))+
  scale_size_manual(values = c(1.2,1,1))+
  scale_color_manual(values=c("magenta2", "cyan2", "yellow3"))+
  scale_y_continuous(labels=percent) +
  scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
  theme(text = element_text(size=20))+
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(
    legend.position = c(.999, .999),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.background = element_rect(fill=alpha('white', 0)))+
  theme(plot.title = element_text(hjust = 0.5))

#-----------------------------------------------------------------------
input_data_500 <- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\edge_data (2).xlsx", 
                             sheet = "Sheet1")

melted_500<-melt(input_data_500,  id.vars = "Date", value.name = "value")
melted_500$Date<-ymd(melted_500$Date)



ggplot() +
  geom_rect(data=sovereign,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="pink", alpha=0.5) +
  geom_rect(data=euro,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Gross number of connections on top 5 nodes \n Window size: 750; Lag: based on AIC")+
  geom_line(data=melted_500, aes(Date, value, colour = variable, size = variable))+
  scale_size_manual(values = c(1.2,1,1))+
  scale_color_manual(values=c("brown", "green2", "snow4"))+
  #scale_y_continuous(labels=percent) +
  scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
  theme(text = element_text(size=20))+
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(
    legend.position = c(.999, .999),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.background = element_rect(fill=alpha('white', 0)))+
  theme(plot.title = element_text(hjust = 0.5))






 input_data_500 <- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\edge_counts.xlsx", 
                             sheet = "Full_percentage_500")

melted_500<-melt(input_data_500,  id.vars = "Date", value.name = "value")
melted_500$Date<-ymd(melted_500$Date)



ggplot() +
  geom_rect(data=sovereign,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="pink", alpha=0.5) +
  geom_rect(data=euro,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Dynamic Toda-Yamamoto Spillover Analysis \n Window size: 500; Lag: based on AIC")+
  geom_line(data=melted_500, aes(Date, value, colour = variable, size = variable))+
  #scale_size_manual(values = c(1.2,1,1))+
  scale_color_manual(values=c("magenta2", "cyan2", "yellow3"))+
  #scale_y_continuous(labels=percent) +
  scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
  theme(text = element_text(size=20))+
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(
    legend.position = c(.999, .999),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.background = element_rect(fill=alpha('white', 0)))+
  theme(plot.title = element_text(hjust = 0.5))

#-----------------------------------------------------------------------

input_data_1000 <- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\edge_counts.xlsx", 
                             sheet = "Full_percentage_1000")

melted_1000<-melt(input_data_1000,  id.vars = "Date", value.name = "value")
melted_1000$Date<-ymd(melted_1000$Date)



ggplot() +
  geom_rect(data=sovereign,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="pink", alpha=0.5) +
  geom_rect(data=euro,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Dynamic Toda-Yamamoto Spillover Analysis \n Window size: 1000; Lag: based on AIC")+
  geom_line(data=melted_1000, aes(Date, value, colour = variable, size = variable))+
  scale_size_manual(values = c(1.2,1,1))+
  scale_color_manual(values=c("magenta2", "cyan2", "yellow3"))+
  scale_y_continuous(labels=percent) +
  scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
  theme(text = element_text(size=20))+
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(
    legend.position = c(.999, .999),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.background = element_rect(fill=alpha('white', 0)))+
  theme(plot.title = element_text(hjust = 0.5))


input_data_granger <- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\edge_counts.xlsx", 
                              sheet = "TY_Granger")

input_data_granger <-input_data_granger[,1:2]



input_data_granger <-tail(input_data_granger,340)

input_data_granger$Granger<-(input_data_granger$Granger-min(input_data_granger$Granger))/
  (max(input_data_granger$Granger)-min(input_data_granger$Granger))

colnames(input_data_granger) <- c("Date","HHI index")

melted_granger<-melt(input_data_granger,  id.vars = "Date", value.name = "value")
melted_granger$Date<-ymd(melted_granger$Date)


ggplot() +
#  geom_rect(data=sovereign,
#            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
#            fill="pink", alpha=0.5) +
#  geom_rect(data=euro,
#            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
#            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Total Network Interconnectedness Measure")+
  geom_line(data=melted_granger, aes(Date, value, colour = variable, size = variable))+
  scale_size_manual(values = c(1,1))+
  scale_color_manual(values=c("black", "magenta"))+
  scale_y_continuous() +
  scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
  theme(text = element_text(size=20))+
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(
    legend.position = c(.999, .999),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.background = element_rect(fill=alpha('white', 0)))+
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------
input_data_levels <- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\factors.xlsx", 
                                sheet = "Levels_norm")

melted_levels<-melt(input_data_levels,  id.vars = "Date", value.name = "value")
melted_levels$Date<-ymd(melted_levels$Date)


ggplot() +
  geom_rect(data=sovereign,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="pink", alpha=0.5) +
  geom_rect(data=euro,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Diebold-Li factor model \n Normalized level factors")+
  geom_line(data=melted_levels, aes(Date, value, color = variable, size = variable))+
  scale_size_manual(values = c(rep(1,20)))+
  #scale_color_manual(values=c("black", "magenta"))+
  scale_y_continuous() +
  scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
  theme(text = element_text(size=20))+
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(
    legend.position = c(1,1),
    legend.direction = "horizontal",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.background = element_rect(fill=alpha('white', 0)))+
  theme(plot.title = element_text(hjust = 0.5))




input_data_slopes <- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\factors.xlsx", 
                                 sheet = "Slopes_norm")

melted_slopes<-melt(input_data_slopes,  id.vars = "Date", value.name = "value")
melted_slopes$Date<-ymd(melted_slopes$Date)


ggplot() +
  geom_rect(data=sovereign,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="pink", alpha=0.5) +
  geom_rect(data=euro,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Diebold-Li factor model \n Normalized slope factors")+
  geom_line(data=melted_slopes, aes(Date, value, color = variable, size = variable))+
  scale_size_manual(values = c(rep(1,20)))+
  #scale_color_manual(values=c("black", "magenta"))+
   scale_y_continuous() +
   scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
   labs(x=NULL, y=NULL) +
   theme_bw() +
   theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
   theme(text = element_text(size=20))+
   theme(panel.grid.minor=element_blank()) +
   theme(panel.grid.major.x=element_blank()) +
   theme(axis.ticks=element_blank())+
   theme(
     legend.position = c(1,1),
     legend.direction = "horizontal",
     legend.justification = c("right", "top"),
     legend.box.just = "right",
     legend.margin = margin(1, 1, 1, 1),
     legend.title=element_blank(),
     legend.text = element_text(size=18),
     legend.background = element_rect(fill=alpha('white', 0)))+
   theme(plot.title = element_text(hjust = 0.5))


input_data_curvatures <- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\factors.xlsx", 
                                sheet = "Curvatures_norm")

melted_curvatures<-melt(input_data_curvatures,  id.vars = "Date", value.name = "value")
melted_curvatures$Date<-ymd(melted_curvatures$Date)


ggplot() +
  geom_rect(data=sovereign,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="pink", alpha=0.5) +
  geom_rect(data=euro,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Diebold-Li factor model \n Normalized curvature factors")+
  geom_line(data=melted_curvatures, aes(Date, value, color = variable, size = variable))+
  scale_size_manual(values = c(rep(1,20)))+
  #scale_color_manual(values=c("black", "magenta"))+
  scale_y_continuous() +
  scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
  theme(text = element_text(size=20))+
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(
    legend.position = c(1,1),
    legend.direction = "horizontal",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.background = element_rect(fill=alpha('white', 0)))+
  theme(plot.title = element_text(hjust = 0.5))


input_data_spearman_inner <- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\spearman.xlsx", 
                                    sheet = "inner")

melted_spearman_inner<-melt(input_data_spearman_inner,  id.vars = "Date", value.name = "value")
melted_spearman_inner$Date<-ymd(melted_spearman_inner$Date)

ggplot() +
  geom_rect(data=sovereign,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="pink", alpha=0.5) +
  geom_rect(data=euro,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Dynamic pairwise Spearman correlation (inner edges) \n Window size: 750; Lag: based on AIC")+
  geom_line(data=melted_spearman_inner, aes(Date, value, colour = variable, size = variable))+
  scale_size_manual(values = c(1,1,1))+
  scale_color_manual(values=c("purple", "yellow", "cyan"))+
  scale_y_continuous() +
  scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
  theme(text = element_text(size=20))+
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(
    legend.position = c(.999,0.001),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.background = element_rect(fill=alpha('white', 0)))+
  theme(plot.title = element_text(hjust = 0.5))


input_data_spearman_all <- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\spearman.xlsx", 
                                        sheet = "all")

melted_spearman_all<-melt(input_data_spearman_all,  id.vars = "Date", value.name = "value")
melted_spearman_all$Date<-ymd(melted_spearman_all$Date)

ggplot() +
  geom_rect(data=sovereign,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="pink", alpha=0.5) +
  geom_rect(data=euro,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Dynamic pairwise Spearman correlation (all edges) \n Window size: 750; Lag: based on AIC")+
  geom_line(data=melted_spearman_all, aes(Date, value, colour = variable, size = variable))+
  scale_size_manual(values = c(1,1,1))+
  scale_color_manual(values=c("purple", "yellow", "cyan"))+
  scale_y_continuous() +
  scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
  theme(text = element_text(size=20))+
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(
    legend.position = c(.999,0.001),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.background = element_rect(fill=alpha('white', 0)))+
  theme(plot.title = element_text(hjust = 0.5))


input_data_unique<- read_excel("D:\\Research_results\\Sovereign_interconnectedness\\Data\\unique-overlapping.xlsx", 
                                 sheet = "union")

melted_unique<-melt(input_data_unique,  id.vars = "Date", value.name = "value")
melted_unique$Date<-ymd(melted_unique$Date)


ggplot() +
  geom_rect(data=sovereign,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="pink", alpha=0.5) +
  geom_rect(data=euro,
            aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
            fill="cornflowerblue", alpha=0.5) +
  labs(title = "Unique and overlapping edges within the sub-networks \n Window size: 750; Lag: based on AIC")+
  geom_line(data=melted_unique, aes(Date, value, colour = variable, size = variable))+
  scale_size_manual(values = c(1,1))+
  scale_color_manual(values=c("black", "magenta"))+
  scale_y_continuous() +
  scale_x_date(expand=c(0,0),date_breaks = "years",date_labels = "%Y")+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1, vjust=0.5)) +
  theme(text = element_text(size=20))+
  theme(panel.grid.minor=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.ticks=element_blank())+
  theme(
    legend.position = c(.999, .999),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=18),
    legend.background = element_rect(fill=alpha('white', 0)))+
  theme(plot.title = element_text(hjust = 0.5))
