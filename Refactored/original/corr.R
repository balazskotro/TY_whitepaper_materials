library("readxl")
sp_329 <- read_excel("C:\\Users\\x\\Desktop\\sp329.xlsx")
df_sp_329<-as.data.frame(sp_329)[,-1]
corr_matrix<-cor(df_sp_329, y = NULL, use = "all.obs", method = c("pearson"))
diag(corr_matrix)<-NA

k<-arrayInd(min(corr_matrix[corr_matrix<0.01 & corr_matrix>0],na.rm=TRUE), dim(corr_matrix))
rownames(corr_matrix)[k[,1]] #WEC
colnames(corr_matrix)[k[,2]] #AEP


k<-arrayInd(which.min( corr_matrix), dim(corr_matrix))
rownames(corr_matrix)[k[,1]] #NVR
colnames(corr_matrix)[k[,2]] #APA

k<-arrayInd(which(corr_matrix==min(corr_matrix[corr_matrix<0.01 & corr_matrix>0],na.rm=TRUE))[1], dim(corr_matrix))
rownames(corr_matrix)[k[,1]] #CTL
colnames(corr_matrix)[k[,2]] #AZO

corr_matrix[c("CTL","AZO","WEC","AEP","NVR","APA"),c("CTL","AZO","WEC","AEP","NVR","APA")]
