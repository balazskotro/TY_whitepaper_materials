plot_matrix<-as.data.frame(matrix(data= 0,5, 5))
rownames(plot_matrix) <-c("all","node","level","slope","curvature")
colnames(plot_matrix) <-c("all","node","level","slope","curvature")



for (i in 1: nrow(as.data.frame(input))) {
  
  if (grepl("/",input[i])==T) {
    sliced_val<- unlist(strsplit(input[i],"/")  )
    r_index<-sliced_val[1] 
    c_index<-sliced_val[2]
    check_p<-gsub("/", "|",input[i])
  } else{
    r_index<-input[i]
    c_index<-input[i]
    check_p<-r_index<-input[i]
  }
  
  
  plot_matrix[r_index,c_index]<-any(lapply(list(colnames(plot_matrix)),grepl, pattern=check_p)[[1]])
  
}

names.fi1 = substr(colnames(global_lambdas),1,3)

#M2<-cbind(as.data.frame(names.fi),result_matrix)

result_matrix <- replace(result_matrix,is.na(result_matrix),0)
diag(result_matrix)<-NA

con1      = as.matrix(result_matrix)
#

con1[con1==0]<-10e-15
diag(con1)<-0

groups1   = list(1:((ncol(result_matrix))/3), 
                ((ncol(result_matrix))/3+1):(((ncol(result_matrix))/3)*2), 
                ((((ncol(result_matrix))/3)*2)+1):(ncol(result_matrix)))
# 


col1      = c(rep("red", ((ncol(result_matrix))/3)), 
             rep("blue", ((ncol(result_matrix))/3)), 
             rep("green4", ((ncol(result_matrix))/3)))

nodecol1=c(rep("white", (ncol(result_matrix))))
# 
con1 = ifelse(abs(con1) <= ((granger_p)), con1, 0)


plotter<-function(con,groups,node_col, col,nodecol,legendstring,legendstring2,repu,layout){
  
  plot_all = qgraph(con, groups = groups, layout = layout, layoutScale = c(1.3, 1.3),normalize=TRUE,
                    maximum = max(con),shape = "circle", mar=c(5,5,5,5),
                    label.font = 35, label.cex = 1.2,labels = names.fi, esize =1,
                    color = nodecol, trans=1,
                    node.width = 1 ,node.height = 0.5, label.color = col,
                    edge.color = col,repulsion=repu,
                    border.width = 2.5, border.color = node_col,asize = 3,curve = 0.8)#,filetype = "pdf", height = 5, width = 10)
  par(cex = 1.8)
  legend=legend("topleft", legend=legendstring,
                col=legendstring2, lty=1:1, pt.cex=30,box.lty=50,text.font=50,lwd=5, bty = "n",y.intersp=1.5)
  #legend("topright",inset=-0.08 , legend=paste0("Date: ",tail(index(global_lambdas),1)),pt.cex=3,box.lty=0,text.font=4,bty = "n")
  #legend("bottomleft", inset=-0.07,legend=paste0("Edges: ",sum((con>0), na.rm = TRUE)))
  #legend("bottomright",inset=-0.1,legend=paste0("Lag order: ",Type_of_inf_criterion, " ", var_order),pt.cex = 3,box.lty=0,text.font=4,bty="n")
}

#----------------------------------------- full net

if (plot_matrix["all", "all"]==1){
names.fi=names.fi1
layout='groups'

legendstring2<-c("red", "blue","green4")
repu=0.9
 con=con1
 groups=groups1
 col=col1
 nodecol=nodecol1
 node_col=col
 legendstring=c(c(paste0("Level (",percent(L1edges_out/288,accuracy = 0.01),") of all edges"), paste0("Slope (",percent(L2edges_out/288,accuracy = 0.01),") of all edges"), paste0("Curvature (",percent(L3edges_out/288,accuracy = 0.01),") of all edges")))
# legendstring1<-c("Level", "Slope", "Curvature")
# legendstring2<-c("red", "blue","green4")
pdf(file = paste0("D:\\Research_results\\Sovereign_interconnectedness\\Plots\\Full_net\\All_plot_",startperiod,"_",endperiod,"_",granger_p,".pdf"),   # The directory you want to save the file in
     width = 16, # The width of the plot in inches
     height = 16)
plotter(con,groups,col,node_col,nodecol,legendstring,legendstring2,repu,layout)
dev.off()
 }
#----------------------------------------- innerempty net

if (plot_matrix["all", "all"]==1){
  names.fi=names.fi1
  level_tags<-as.numeric(unlist(groups1[[1]]))
  slope_tags<-as.numeric(unlist(groups1[[2]]))
  curvature_tags<-as.numeric(unlist(groups1[[3]]))
  layout='groups'
  legendstring=c(c(paste0("Level (",percent(L1edges_out/288,accuracy = 0.01),") of all edges"), paste0("Slope (",percent(L2edges_out/288,accuracy = 0.01),") of all edges"), paste0("Curvature (",percent(L3edges_out/288,accuracy = 0.01),") of all edges")))
  legendstring2<-c("red", "blue","green4")
  repu=0.9
  con=con1
  con[level_tags,level_tags]<-0
  con[slope_tags,slope_tags]<-0
  con[curvature_tags,curvature_tags]<-0
  groups=groups1
  col=col1
  nodecol=nodecol1
  node_col=col
  pdf(file = paste0("D:\\Research_results\\Sovereign_interconnectedness\\Plots\\Inner_empty\\All_plot_innerempty_",startperiod,"_",endperiod,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off()
}
#----------------------------------------- only level net

if (plot_matrix["all", "all"]==1){
  names.fi=names.fi1
  level_tags<-as.numeric(unlist(groups1[[1]]))
  slope_tags<-as.numeric(unlist(groups1[[2]]))
  curvature_tags<-as.numeric(unlist(groups1[[3]]))
  layout='groups'
  legendstring=c(paste0("Level (",percent(L1edges/132,accuracy = 0.01),") of all edges"), "Slope", "Curvature")
  legendstring2<-c("red", "#0000FF33","#008B0033")
  repu=0.9
  con=con1
  #con[level_tags,level_tags]<-toplot_l1
  con[slope_tags,slope_tags]<-0
  con[curvature_tags,curvature_tags]<-0
  con[level_tags,slope_tags]<-0
  con[slope_tags,level_tags]<-0
  con[level_tags,curvature_tags]<-0
  con[curvature_tags,level_tags]<-0
  con[curvature_tags,slope_tags]<-0
  con[slope_tags,curvature_tags]<-0
  groups=groups1
  col= c(rep("red", ((ncol(result_matrix))/3)), 
          rep("#0000FF33", ((ncol(result_matrix))/3)), 
          rep("#008B0033", ((ncol(result_matrix))/3)))
  nodecol=nodecol1
  node_col=col
  pdf(file = paste0("D:\\Research_results\\Sovereign_interconnectedness\\Plots\\Levels_only\\All_plot_onlylevel",startperiod,"_",endperiod,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off()
}


if (plot_matrix["all", "all"]==1){
  names.fi=names.fi1
  level_tags<-as.numeric(unlist(groups1[[1]]))
  slope_tags<-as.numeric(unlist(groups1[[2]]))
  curvature_tags<-as.numeric(unlist(groups1[[3]]))
  layout='groups'
  legendstring=c("Level", paste0("Slope (",percent(L2edges/132,accuracy = 0.01),") of all edges"), "Curvature")
  legendstring2<-c("#FF000033", "blue","#008B0033")
  repu=0.9
  con=con1
  con[level_tags,level_tags]<-0
  con[curvature_tags,curvature_tags]<-0
  con[level_tags,slope_tags]<-0
  con[level_tags,curvature_tags]<-0
  con[curvature_tags,level_tags]<-0
  con[curvature_tags,slope_tags]<-0
  con[slope_tags,curvature_tags]<-0
  con[slope_tags,level_tags]<-0
  groups=groups1
  col= c(rep("#FF000033", ((ncol(result_matrix))/3)), 
         rep("blue", ((ncol(result_matrix))/3)), 
         rep("#008B0033", ((ncol(result_matrix))/3)))
  nodecol=nodecol1
  node_col=col
  pdf(file = paste0("D:\\Research_results\\Sovereign_interconnectedness\\Plots\\Slopes_only\\All_plot_onlyslope_",startperiod,"_",endperiod,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off()
}


if (plot_matrix["all", "all"]==1){
  names.fi=names.fi1
  level_tags<-as.numeric(unlist(groups1[[1]]))
  slope_tags<-as.numeric(unlist(groups1[[2]]))
  curvature_tags<-as.numeric(unlist(groups1[[3]]))
  layout='groups'
  legendstring=c("Level", "Slope", paste0("Curvature (",percent(L3edges/132,accuracy = 0.01),") of all edges"))
  legendstring2<-c("#FF000033", "#0000FF33","green4")
  repu=0.9
  con=con1
  con[level_tags,level_tags]<-0
  con[slope_tags,slope_tags]<-0
  con[level_tags,slope_tags]<-0
  con[level_tags,curvature_tags]<-0
  con[curvature_tags,level_tags]<-0
  con[curvature_tags,slope_tags]<-0
  con[slope_tags,curvature_tags]<-0
  con[slope_tags,level_tags]<-0
  groups=groups1
  col= c(rep("#FF000033", ((ncol(result_matrix))/3)), 
         rep("#0000FF33", ((ncol(result_matrix))/3)), 
         rep("green4", ((ncol(result_matrix))/3)))
  nodecol=nodecol1
  node_col=col
  pdf(file = paste0("D:\\Research_results\\Sovereign_interconnectedness\\Plots\\Curvatures_only\\All_plot_onlycurv_",startperiod,"_",endperiod,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off()
}






if (plot_matrix["level", "level"]==1){
  level_tags<-as.numeric(unlist(groups1[[1]]))
  legendstring=c("Level")
  layout='groups'
  legendstring2<-c("red")
  repu=0.9
  names.fi=names.fi1[level_tags]
  con=con1[level_tags,level_tags]
  groups=groups1[1]
  col=col1[level_tags]
  nodecol=nodecol1[level_tags]
  legendstring1<-c("Level")
  legendstring2<-c("red")
  node_col=col
  pdf(file = paste0("Level_plot_",start_date,"_",end_date,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off()
}

if (plot_matrix["slope", "slope"]==1){
  slope_tags<-as.numeric(unlist(groups1[[2]]))
  legendstring=c("Slope")
  layout='groups'
  legendstring2<-c("blue")
  repu=0.9
  names.fi=names.fi1[slope_tags]
  con=con1[slope_tags,slope_tags]
  groups=groups1[1]
  col=col1[slope_tags]
  nodecol=nodecol1[slope_tags]
  node_col=col
  legendstring1<-c("Slope")
  legendstring2<-c("blue")
  pdf(file = paste0("Slope_plot_",start_date,"_",end_date,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off()
}

if (plot_matrix["curvature", "curvature"]==1){
  curvature_tags<-as.numeric(unlist(groups1[[3]]))
  names.fi=names.fi1[curvature_tags]
  repu=0.9
  layout='groups'
  legendstring=c("Curvature")
  con=con1[curvature_tags,curvature_tags]
  groups=groups1[1]
  col=col1[curvature_tags]
  nodecol=nodecol1[curvature_tags]
  node_col=col
  legendstring2<-c("green4")
  pdf(file = paste0("Curvature_plot_",start_date,"_",end_date,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off()
  
}

if (plot_matrix["level", "slope"]==1 || plot_matrix["slope", "level"]==1 ){

ls_tags<-as.numeric(unlist(groups1[c(1,2)]))
names.fi=names.fi1[ls_tags]
con=con1[ls_tags,ls_tags]
groups=groups1[c(1,2)]
layout='circle'
col=col1[ls_tags]
repu=0.6
nodecol=nodecol1[ls_tags]
legendstring<-c("Level","Slope")
legendstring2<-c("red","blue")
node_col=col
pdf(file = paste0("Level-Slope_plot_",start_date,"_",end_date,"_",granger_p,".pdf"),
    width = 16, # The width of the plot in inches
    height = 16)
plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
dev.off()

}


if (plot_matrix["level", "curvature"]==1 || plot_matrix["curvature", "level"]==1 ){
  
  ls_tags<-as.numeric(unlist(groups1[c(1,3)]))
  names.fi=names.fi1[ls_tags]
  con=con1[ls_tags,ls_tags]
  layout='circle'
  repu=0.6
  groups=groups1[c(1,2)]
  col=col1[ls_tags]
  nodecol=nodecol1[ls_tags]
  legendstring<-c("Level","Curvature")
  legendstring2<-c("red","green4")
  node_col=col
  pdf(file = paste0("Level-Curvature_plot_",start_date,"_",end_date,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off()
  
}

if (plot_matrix["slope", "curvature"]==1 || plot_matrix["curvature", "slope"]==1 ){
  
  ls_tags<-as.numeric(unlist(groups1[c(2,3)]))
  names.fi=names.fi1[ls_tags]
  con=con1[ls_tags,ls_tags]
  layout='circle'
  repu=0.6
  groups=groups1[c(1,2)]
  col=col1[ls_tags]
  nodecol=nodecol1[ls_tags]
  legendstring<-c("Slope","Curvature")
  legendstring2<-c("blue","green4")
  node_col=col
  pdf(file = paste0("Slope-Curvature_plot_",start_date,"_",end_date,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off() 
  
}


if (plot_matrix["all", "all"]==1){
  names.fi=names.fi1
  level_tags<-as.numeric(unlist(groups1[[1]]))
  slope_tags<-as.numeric(unlist(groups1[[2]]))
  curvature_tags<-as.numeric(unlist(groups1[[3]]))
  layout='groups'
  legendstring=c(paste0("Level (",percent(sum(toplot_l1)/132,accuracy = 0.01),") of all edges"),
                 paste0("Slope (",percent(sum(toplot_l2)/132,accuracy = 0.01),") of all edges") , 
                 paste0("Curvature (",percent(sum(toplot_l3)/132,accuracy = 0.01),") of all edges"))
  legendstring2<-c("red", "blue","green4")
  repu=0.9
  con=con1
  con[level_tags,level_tags]<-toplot_l1
  con[slope_tags,slope_tags]<-toplot_l2
  con[curvature_tags,curvature_tags]<-toplot_l3
  con[level_tags,slope_tags]<-0
  con[slope_tags,level_tags]<-0
  con[level_tags,curvature_tags]<-0
  con[curvature_tags,level_tags]<-0
  con[curvature_tags,slope_tags]<-0
  con[slope_tags,curvature_tags]<-0
  groups=groups1
  col= c(rep("red", ((ncol(result_matrix))/3)), 
         rep("blue", ((ncol(result_matrix))/3)), 
         rep("green4", ((ncol(result_matrix))/3)))
  nodecol=nodecol1
  node_col=col
  pdf(file = paste0("D:\\Research_results\\Sovereign_interconnectedness\\Plots\\Unique_edges\\unique_edges",startperiod,"_",endperiod,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off()
}




if (plot_matrix["all", "all"]==1){
  names.fi=names.fi1
  level_tags<-as.numeric(unlist(groups1[[1]]))
  slope_tags<-as.numeric(unlist(groups1[[2]]))
  curvature_tags<-as.numeric(unlist(groups1[[3]]))
  layout='groups'
  legendstring=c(paste0("Level (",percent(sum(only_three)/132,accuracy = 0.01),") of all edges"),
                 paste0("Slope (",percent(sum(only_three)/132,accuracy = 0.01),") of all edges") , 
                 paste0("Curvature (",percent(sum(only_three)/132,accuracy = 0.01),") of all edges"))
  legendstring2<-c("red", "blue","green4")
  repu=0.9
  con=con1
  con[level_tags,level_tags]<-only_three
  con[slope_tags,slope_tags]<-only_three
  con[curvature_tags,curvature_tags]<-only_three
  con[level_tags,slope_tags]<-0
  con[slope_tags,level_tags]<-0
  con[level_tags,curvature_tags]<-0
  con[curvature_tags,level_tags]<-0
  con[curvature_tags,slope_tags]<-0
  con[slope_tags,curvature_tags]<-0
  groups=groups1
  col= c(rep("red", ((ncol(result_matrix))/3)), 
         rep("blue", ((ncol(result_matrix))/3)), 
         rep("green4", ((ncol(result_matrix))/3)))
  nodecol=nodecol1
  node_col=col
  pdf(file = paste0("D:\\Research_results\\Sovereign_interconnectedness\\Plots\\Overlapping_edges\\overlapping_edges",startperiod,"_",endperiod,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,col,node_col, nodecol,legendstring,legendstring2,repu,layout)
  dev.off()
}





if (plot_matrix["node", "node"]==1){
  names.fi=names.fi1
  clone<-(matrix(data= 0,nrow = nrow(con1) , ncol = ncol(con1)))
  repu=0.9
  layout='groups'
  rownames(clone) <-rownames(con1)
  colnames(clone) <-colnames(con1)
  
  clone[,nodeident]<-con1[,nodeident]
  clone[nodeident,]<-con1[nodeident,]

  con=clone
  groups=groups1
  
  col_t      = c(rep("#FF000033", ((ncol(result_matrix))/3)), 
                 rep("#0000FF33", ((ncol(result_matrix))/3)), 
                 rep("#008B0033", ((ncol(result_matrix))/3)))
  
  
    
  col<-apply(clone, 1, function(x){ifelse(all(x == 0), "#dummy", NA)})==
    apply(clone, 2, function(x){ifelse(all(x == 0), "#dummy", NA)})

  col[which(col)]<-col_t[which(col)]
  col[is.na(col)]<-col1[is.na(col)]
  #col[labels(col)==nodeident]<-"black"
  
  nodecol=nodecol1
  node_col=col
  node_col[labels(node_col)==nodeident]<-"black"
  
  if (substr(nodeident,7,7)==1) {
    
  
  sum_edges<-incoming_edges$`apply(edge_counter, 2, sum)`[rownames(incoming_edges)==paste0(nodeident,"_inner")]+outgoing_edges$`apply(edge_counter, 1, sum)`[rownames(outgoing_edges)==paste0(nodeident,"_outer")]
  
  legendstring=c(paste0("Level (",percent(sum_edges/70,accuracy = 0.01),") of all edges"), "Slope", "Curvature")
  
  
  } else if (substr(nodeident,7,7)==2) {
    
    sum_edges<-incoming_edges$`apply(edge_counter, 2, sum)`[rownames(incoming_edges)==paste0(nodeident,"_inner")]+outgoing_edges$`apply(edge_counter, 1, sum)`[rownames(outgoing_edges)==paste0(nodeident,"_outer")]
    
    legendstring=c("Level", paste0("Slope (",percent(sum_edges/70,accuracy = 0.01),") of all edges"), "Curvature")
    
  } else if  (substr(nodeident,7,7)==3) {
    
    sum_edges<-incoming_edges$`apply(edge_counter, 2, sum)`[rownames(incoming_edges)==paste0(nodeident,"_inner")]+outgoing_edges$`apply(edge_counter, 1, sum)`[rownames(outgoing_edges)==paste0(nodeident,"_outer")]
    
    legendstring=c("Level", "Slope", paste0("Curvature (",percent(sum_edges/70,accuracy = 0.01),") of all edges"))
  }
  
  legendstring2<-c("red", "blue","green4")
  
  pdf(file = paste0("D:\\Research_results\\Sovereign_interconnectedness\\Plots\\Nodes\\",nodeident,"_plot_",start_date,"_",end_date,"_",granger_p,".pdf"),
      width = 16, # The width of the plot in inches
      height = 16)
  plotter(con,groups,node_col, col,nodecol,legendstring,legendstring2,repu,layout)
  dev.off() 
}


