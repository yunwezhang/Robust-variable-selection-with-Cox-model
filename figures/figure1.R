
# this is to get figure1

library(ggplot2)

####all auxilary functions
summary_table_fun=function(run_result,design_params){
  my_list=sapply(run_result,function(x) x[[1]],simplify = FALSE)
  
  # Extract all unique names
  all_names <- unique(unlist(lapply(my_list, "[",1)))
  
  # Create a matrix with 0s
  matrix_data <- matrix(0, nrow = length(my_list), ncol = length(all_names), dimnames = list(names(my_list), all_names))
  
  # Fill in the matrix with values from the named vectors
  for (i in seq_along(my_list)) {
    temp_dt=as.data.frame(my_list[[i]])
    names_i <- temp_dt$Name
    matrix_data[i, names_i] <- temp_dt$Value
  }
  
  # Function to fill NA with 0 and convert to numeric
  fill_na_convert_numeric <- function(x) {
    x[is.na(x)] <- 0
    as.numeric(x)
  }
  
  # Apply the function to each column of the matrix
  matrix_data <- apply(matrix_data, 2, fill_na_convert_numeric)
  
  # Convert the matrix to a data frame
  result_df <- as.data.frame(matrix_data)
  
  # delete zeros for final results
  final_df=round(colMeans(result_df,na.rm = TRUE),2)
  final_df=final_df[final_df!=0]
  
  # get abs diff
  # #vectors_to_subtract=data.frame(X1=1,X2=1,X3=1,X4=1,X5=1,X6=1,X30=1)
  # vectors_to_subtract=design_params
  # columns_to_subtract <- names(vectors_to_subtract)
  # col_name=colnames(result_df)
  # data=as.data.frame(matrix(0,nrow=nrow(result_df),ncol=length(columns_to_subtract)))
  # colnames(data)=columns_to_subtract
  # # Subtract vectors from corresponding columns
  # for (i in 1:length(columns_to_subtract)) {
  #    col_index <- which(colnames(result_df) == col_name[i])
  #   data[, col_index] <- abs(result_df[, col_index] - vectors_to_subtract[[col_name[i]]])
  # }
  # final_df2=colSums(data,na.rm = TRUE)
  final_df2=data.frame(matrix(nrow=nrow(design_params),ncol = ncol(design_params)))
  vectors_to_subtract=design_params
  for(i in 1:length(colnames(design_params))){
    if(length(final_df)==0){final_df2=NA}else if(length(which(names(final_df) == colnames(design_params)[i]))==0){
      final_df2[1,i]=abs(design_params[i])
    }else{
      final_df2[1,i]=abs(final_df[which(names(final_df) == colnames(design_params)[i])]-design_params[i])}
  }
  #colnames(final_df2)=colnames(design_params)
  # get squared error
  # #vectors_to_subtract=data.frame(X1=1,X2=1,X3=1,X4=1,X5=1,X6=1,X30=1)
  # vectors_to_subtract=design_params
  # columns_to_subtract <- names(vectors_to_subtract)
  # col_name=colnames(result_df)
  # data=as.data.frame(matrix(0,nrow=nrow(result_df),ncol=length(columns_to_subtract)))
  # colnames(data)=columns_to_subtract
  # # Subtract vectors from corresponding columns
  # for (i in 1:length(columns_to_subtract)) {
  #    col_index <- which(colnames(result_df) == col_name[i])
  #   data[, col_index] <- (result_df[, col_index] - vectors_to_subtract[[col_name[i]]])^2
  # }
  # final_df3=colSums(data,na.rm = TRUE)
  final_df3=data.frame(matrix(nrow=nrow(design_params),ncol = ncol(design_params)))
  vectors_to_subtract=design_params
  for(i in 1:length(colnames(design_params))){
    if(length(final_df)==0){final_df3=NA}else if(length(which(names(final_df) == colnames(design_params)[i]))==0){
      final_df3[1,i]=abs(design_params[i])^2
    }else{
      final_df3[1,i]=(final_df[which(names(final_df) == colnames(design_params)[i])]-design_params[i])^2}
  }
  #colnames(final_df3)=colnames(design_params)
  return(list(result_df,final_df,final_df2,final_df3))
}

summary_table_fun2=function(run_result,design_params){
  my_list=sapply(run_result,function(x) x[[1]],simplify = FALSE)
  
  # Extract all unique names
  all_names <- unique(unlist(lapply(my_list, names)))
  
  # Create a matrix with 0s
  matrix_data <- matrix(0, nrow = length(my_list), ncol = length(all_names), dimnames = list(names(my_list), all_names))
  
  # Fill in the matrix with values from the named vectors
  for (i in seq_along(my_list)) {
    names_i <- names(my_list[[i]])
    matrix_data[i, names_i] <- my_list[[i]]
  }
  
  # Function to fill NA with 0 and convert to numeric
  fill_na_convert_numeric <- function(x) {
    x[is.na(x)] <- 0
    as.numeric(x)
  }
  
  # Apply the function to each column of the matrix
  matrix_data <- apply(matrix_data, 2, fill_na_convert_numeric)
  
  # Convert the matrix to a data frame
  result_df <- as.data.frame(matrix_data)
  
  # delete zeros for final results
  final_df=round(colMeans(result_df,na.rm = TRUE),2)
  final_df=final_df[final_df!=0]
  # get abs diff
  # #vectors_to_subtract=data.frame(X1=1,X2=1,X3=1,X4=1,X5=1,X6=1,X30=1)
  # vectors_to_subtract=design_params
  # columns_to_subtract <- names(vectors_to_subtract)
  # col_name=colnames(result_df)
  # data=as.data.frame(matrix(0,nrow=nrow(result_df),ncol=length(columns_to_subtract)))
  # colnames(data)=columns_to_subtract
  # # Subtract vectors from corresponding columns
  # for (i in 1:length(columns_to_subtract)) {
  #    col_index <- which(colnames(result_df) == col_name[i])
  #   data[, col_index] <- abs(result_df[, col_index] - vectors_to_subtract[[col_name[i]]])
  # }
  # final_df2=colSums(data,na.rm = TRUE)
  final_df2=data.frame(matrix(nrow=nrow(design_params),ncol = ncol(design_params)))
  vectors_to_subtract=design_params
  for(i in 1:length(colnames(design_params))){
    if(length(final_df)==0){final_df2=NA}else if(length(which(names(final_df) == colnames(design_params)[i]))==0){
      final_df2[1,i]=abs(design_params[i])
    }else{
      final_df2[1,i]=abs(final_df[which(names(final_df) == colnames(design_params)[i])]-design_params[i])}
  }
  #colnames(final_df2)=colnames(design_params)
  # get squared error
  # #vectors_to_subtract=data.frame(X1=1,X2=1,X3=1,X4=1,X5=1,X6=1,X30=1)
  # vectors_to_subtract=design_params
  # columns_to_subtract <- names(vectors_to_subtract)
  # col_name=colnames(result_df)
  # data=as.data.frame(matrix(0,nrow=nrow(result_df),ncol=length(columns_to_subtract)))
  # colnames(data)=columns_to_subtract
  # # Subtract vectors from corresponding columns
  # for (i in 1:length(columns_to_subtract)) {
  #    col_index <- which(colnames(result_df) == col_name[i])
  #   data[, col_index] <- (result_df[, col_index] - vectors_to_subtract[[col_name[i]]])^2
  # }
  # final_df3=colSums(data,na.rm = TRUE)
  final_df3=data.frame(matrix(nrow=nrow(design_params),ncol = ncol(design_params)))
  vectors_to_subtract=design_params
  for(i in 1:length(colnames(design_params))){
    if(length(final_df)==0){final_df3=NA}else if(length(which(names(final_df) == colnames(design_params)[i]))==0){
      final_df3[1,i]=abs(design_params[i])^2
    }else{
      final_df3[1,i]=(final_df[which(names(final_df) == colnames(design_params)[i])]-design_params[i])^2}
  }
  #colnames(final_df3)=colnames(design_params)
  
  return(list(result_df,final_df,final_df2,final_df3))
}


calculation_fun=possibly(function(result,given_names){
  list_of_dataframes<-list()
  for (i in 1:100){
    list_of_dataframes[[i]]=result[[i]][[1]]
  }
  
  # Extract the first column ('characteristics') from each dataframe and skip if an error occurs
  character_columns <- lapply(list_of_dataframes, function(df) {
    tryCatch({
      return(df$Name)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Remove NULL elements
  character_columns <- character_columns[!sapply(character_columns, is.null)]
  
  # get model size
  model_size=sapply(character_columns,length)
  
  #get true mdoel proportion
  true_num=0
  for(i in 1:length(character_columns)){
    vec1=sort(character_columns[[i]])
    vec2=sort(given_names)
    equal=all(vec1==vec2)
    if(equal==TRUE){
      true_num=true_num+1
    }else{true_num=true_num}
  }
  # Combine character columns into a single vector
  all_characters <- unlist(character_columns)
  
  # Count occurrences of each character
  character_counts <- table(all_characters)
  
  #update 0315:all counts >half are determied as selected
  character_counts_vector <- names(character_counts[character_counts>50])
  # Function to calculate TP, FP, and F1 score
  calculate_tp_fp_f1 <- function(true_names, predicted_names) {
    # Calculate TP and FP
    tp <- sum(predicted_names %in% true_names)
    fp <- sum(!(predicted_names %in% true_names))
    
    # Calculate F1 score
    precision <- tp / (tp + fp)
    recall <- tp / length(true_names)
    
    # Handle cases where precision + recall is 0
    if (precision + recall == 0) {
      return(list(tp = tp, fp = fp, f1_score = 0))
    }
    
    f1_score <- 2 * (precision * recall) / (precision + recall)
    
    return(list(tp = tp, fp = fp, f1_score = f1_score))
  }
  
  #given_names=colnames(dd)
  # Calculate TP, FP, and F1 score based on given names and character counts
  result <- calculate_tp_fp_f1(given_names, character_counts_vector)
  
  return(list(result,character_counts,model_size,true_num))}, otherwise=list(list(0,0,0),0,0,0))


calculation_fun2=possibly(function(result,given_names){
  list_of_dataframes<-list()
  for (i in 1:100){
    list_of_dataframes[[i]]=result[[i]][[1]]
  }
  
  # Extract the first column ('characteristics') from each dataframe and skip if an error occurs
  character_columns <- lapply(list_of_dataframes, function(df) {
    tryCatch({
      return(names(df))
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Remove NULL elements
  character_columns <- character_columns[!sapply(character_columns, is.null)]
  
  # get model size
  model_size=sapply(character_columns,length)
  
  #get true mdoel proportion
  true_num=0
  for(i in 1:length(character_columns)){
    vec1=sort(character_columns[[i]])
    vec2=sort(given_names)
    equal=all(vec1==vec2)
    if(equal==TRUE){
      true_num=true_num+1
    }else{true_num=true_num}
  }
  
  # Combine character columns into a single vector
  all_characters <- unlist(character_columns)
  
  # Count occurrences of each character
  character_counts <- table(all_characters)
  
  #update 0315:all counts >half are determied as selected
  character_counts_vector <- names(character_counts[character_counts>50])
  # Function to calculate TP, FP, and F1 score
  calculate_tp_fp_f1 <- function(true_names, predicted_names) {
    # Calculate TP and FP
    tp <- sum(predicted_names %in% true_names)
    fp <- sum(!(predicted_names %in% true_names))
    
    # Calculate F1 score
    precision <- tp / (tp + fp)
    recall <- tp / length(true_names)
    
    # Handle cases where precision + recall is 0
    if (precision + recall == 0) {
      return(list(tp = tp, fp = fp, f1_score = 0))
    }
    
    f1_score <- 2 * (precision * recall) / (precision + recall)
    
    return(list(tp = tp, fp = fp, f1_score = f1_score))
  }
  
  #given_names=colnames(dd)
  # Calculate TP, FP, and F1 score based on given names and character counts
  result <- calculate_tp_fp_f1(given_names, character_counts_vector)
  
  return(list(result,character_counts,model_size,true_num))}, otherwise=list(list(0,0,0),0,0,0))

#####pawph
###############
###############
##### function to get figures
get_figures_each_less_reorder=function(beta, colnames_dt, given_names, scenario1,designed_params,name)
{
  load(scenario1)
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun(result3,given_names),calculation_fun(result4,given_names),
                    calculation_fun(result5,given_names),calculation_fun(result6,given_names),
                    calculation_fun(result7,given_names),calculation_fun(result8,given_names),
                    calculation_fun2(result9,given_names),calculation_fun2(result10,given_names),
                    calculation_fun2(result11,given_names),calculation_fun2(result12,given_names))
  final_cal_table=as.data.frame(matrix(0,nrow=length(all_cal_list),ncol=3))
  for(i in 1:length(all_cal_list)){
    for(j in 1:3){
      final_cal_table[i,j]=all_cal_list[[i]][[1]][[j]]
    }
  }
  highdim_param1_f1=final_cal_table$V3
  highdim_param1_tp=final_cal_table$V1
  highdim_param1_fp=final_cal_table$V2
  
  highdim_param1_correctprob=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_correctprob[i]=all_cal_list[[i]][[4]]/100
  }
  
  highdim_param1_medianmodelsize=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_medianmodelsize[i]=median(all_cal_list[[i]][[3]])
  }
  
  return_table=cbind.data.frame(highdim_param1_tp,highdim_param1_fp,highdim_param1_f1,highdim_param1_correctprob,highdim_param1_medianmodelsize)
  colnames(return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  return_table$method=factor(c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"),levels = c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun(result3,designed_params),summary_table_fun(result4,designed_params),summary_table_fun(result5,designed_params),summary_table_fun(result6,designed_params),summary_table_fun(result7,designed_params),summary_table_fun(result8,designed_params),summary_table_fun2(result9,designed_params),summary_table_fun2(result10,designed_params),summary_table_fun2(result11,designed_params),summary_table_fun2(result12,designed_params))
  mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = length(beta)))
  for(i in 1:length(all_list2)){
    mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
  }
  mean_est$method=factor(c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"),levels = c("scad","snet","ment","mcp","aenet","alasso","enet","lasso","SIS","pawph_scad","pawph_mcp","pawph"))
  highdim_param1=sweep(mean_est[,1:3],2,beta)
  highdim_param1$method=factor(c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"),levels = c("scad","snet","ment","mcp","aenet","alasso","enet","lasso","SIS","pawph_scad","pawph_mcp","pawph"))
  sum_abs=data.frame(matrix(NA,nrow = length(all_list2),ncol = length(beta)))
  for(i in 1:length(all_list2)){
    sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
  }
  sum_abs$method=factor(c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"),levels = c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"))
  mse=data.frame(matrix(NA,nrow = length(all_list2),ncol = length(beta)))
  for(i in 1:length(all_list2)){
    mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
  }
  mse$method=factor(c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"),levels = c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"))
  ########
  
  gg_heatmap_data <- melt(highdim_param1, id.vars = c("method"), variable.name = "variables")
  ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
    geom_tile(color = "grey")+scale_fill_gradientn(limits = c(-3,3),colours = c("#2166AC", "#67A9CF" ,"#D1E5F0", "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Estimated params - true params')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
    geom_text(aes(label = round(value,2)), color = "black", size = 4)
  p1=ggheatmap
  
  gg_heatmap_data <- melt(sum_abs, id.vars = c("method"), variable.name = "variables")
  ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
    geom_tile(color = "grey")+scale_fill_gradientn(limits = c(0,max(sum_abs[,1:3],na.rm = TRUE)),colours = c( "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Estimated params - true params')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
    geom_text(aes(label = round(value,2)), color = "black", size = 4)
  p2=ggheatmap
  
  gg_heatmap_data <- melt(mse, id.vars = c("method"), variable.name = "variables")
  ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
    geom_tile(color = "grey")+scale_fill_gradientn(limits = c(0,max(mse[,1:3],na.rm = TRUE)),colours = c( "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Estimated params - true params')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
    geom_text(aes(label = round(value,2)), color = "black", size = 4)
  p3=ggheatmap
  
  finalgg=ggarrange(p1,p2,p3,nrow=3,ncol = 1)
  finalgg
  ggsave(plot=finalgg,filename=paste(name,"heatmap_less.pdf",sep="_"),height = 10,width = 10)
  
  return(list(return_table,mean_est,sum_abs,mse))
}




get_figures_each_less_reorder(beta=c(1,2,-1), colnames_dt=c("X1","X2","X3"), given_names=c("X1","X2","X3"), scenario1="saved_rds/pawphsimdata_lowdim_outlier_0320.RData",designed_params=data.frame(X1=1,X2=2,X3=-1),name = "pawphlowdimoutlier_revision")
get_figures_each_less_reorder(beta=c(1,2,-1), colnames_dt=c("X1","X2","X3"), given_names=c("X1","X2","X3"), scenario1="saved_rds/pawphsimdata_lowdim_0320.RData",designed_params=data.frame(X1=1,X2=2,X3=-1),name = "pawphlowdim_revision")


#############SIS
##########
#########
get_figures_each_sis_less_reorder=function(beta, colnames_dt, given_names, scenario1,designed_params,name)
{
  load(scenario1)
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun(result3,given_names),calculation_fun(result4,given_names),
                    calculation_fun(result5,given_names),calculation_fun(result6,given_names),
                    calculation_fun(result7,given_names),calculation_fun(result8,given_names),
                    calculation_fun2(result9,given_names),calculation_fun2(result10,given_names),
                    calculation_fun2(result11,given_names),calculation_fun2(result12,given_names))
  final_cal_table=as.data.frame(matrix(0,nrow=length(all_cal_list),ncol=3))
  for(i in 1:length(all_cal_list)){
    for(j in 1:3){
      final_cal_table[i,j]=all_cal_list[[i]][[1]][[j]]
    }
  }
  highdim_param1_f1=final_cal_table$V3
  highdim_param1_tp=final_cal_table$V1
  highdim_param1_fp=final_cal_table$V2
  
  highdim_param1_correctprob=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_correctprob[i]=all_cal_list[[i]][[4]]/100
  }
  
  highdim_param1_medianmodelsize=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_medianmodelsize[i]=median(all_cal_list[[i]][[3]])
  }
  
  return_table=cbind.data.frame(highdim_param1_tp,highdim_param1_fp,highdim_param1_f1,highdim_param1_correctprob,highdim_param1_medianmodelsize)
  colnames(return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  return_table$method=factor(c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"),levels = c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun(result3,designed_params),summary_table_fun(result4,designed_params),summary_table_fun(result5,designed_params),summary_table_fun(result6,designed_params),summary_table_fun(result7,designed_params),summary_table_fun(result8,designed_params),summary_table_fun2(result9,designed_params),summary_table_fun2(result10,designed_params),summary_table_fun2(result11,designed_params),summary_table_fun2(result12,designed_params))
  mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = length(beta)))
  for(i in 1:length(all_list2)){
    mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
  }
  mean_est$method=factor(c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"),levels = c("scad","snet","ment","mcp","aenet","alasso","enet","lasso","SIS","pawph_scad","pawph_mcp","pawph"))
  highdim_param1=sweep(mean_est[,1:length(beta)],2,beta)
  highdim_param1$method=factor(c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"),levels = c("scad","snet","ment","mcp","aenet","alasso","enet","lasso","SIS","pawph_scad","pawph_mcp","pawph"))
  sum_abs=data.frame(matrix(NA,nrow = length(all_list2),ncol = length(beta)))
  for(i in 1:length(all_list2)){
    sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
  }
  sum_abs$method=factor(c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"),levels = c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"))
  mse=data.frame(matrix(NA,nrow = length(all_list2),ncol = length(beta)))
  for(i in 1:length(all_list2)){
    mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
  }
  mse$method=factor(c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"),levels = c("lasso","enet","alasso","aenet","mcp","scad","ment","snet","pawph","pawph_mcp","pawph_scad","SIS"))
  ########
  
  gg_heatmap_data <- melt(highdim_param1, id.vars = c("method"), variable.name = "variables")
  ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
    geom_tile(color = "grey")+scale_fill_gradientn(limits = c(-3,3),colours = c("#2166AC", "#67A9CF" ,"#D1E5F0", "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Estimated params - true params')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
    geom_text(aes(label = round(value,2)), color = "black", size = 4)
  p1=ggheatmap
  
  gg_heatmap_data <- melt(sum_abs, id.vars = c("method"), variable.name = "variables")
  ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
    geom_tile(color = "grey")+scale_fill_gradientn(limits = c(0,max(sum_abs[,1:length(beta)],na.rm = TRUE)),colours = c( "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Estimated params - true params')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
    geom_text(aes(label = round(value,2)), color = "black", size = 4)
  p2=ggheatmap
  
  gg_heatmap_data <- melt(mse, id.vars = c("method"), variable.name = "variables")
  ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
    geom_tile(color = "grey")+scale_fill_gradientn(limits = c(0,max(mse[,1:length(beta)],na.rm = TRUE)),colours = c( "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Estimated params - true params')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
    geom_text(aes(label = round(value,2)), color = "black", size = 4)
  p3=ggheatmap
  
  finalgg=ggarrange(p1,p2,p3,nrow=3,ncol = 1)
  finalgg
  ggsave(plot=finalgg,filename=paste(name,"heatmap_less.pdf",sep="_"),height = 10,width = 10)
  
  return(list(return_table,mean_est,sum_abs,mse))
}

get_figures_each_sis_less_reorder(beta=c(-1.6328,1.3988,-1.6497,1.6353,-1.4209,1.7022), colnames_dt=c("X1","X2","X3","X4","X5","X6"), given_names=c("X1","X2","X3","X4","X5","X6"), scenario1="saved_rds/sissimdata1_0813.RData",designed_params=data.frame(X1=-1.6328,X2=1.3988,X3=-1.6497,X4=1.6353,X5=-1.4209,X6=1.7022),name = "sis_scenario1_reivisionv2")
get_figures_each_sis_less_reorder(beta=c(4,4,4,-6*sqrt(2)), colnames_dt=c("X1","X2","X3","X4"), given_names=c("X1","X2","X3","X4"), scenario1="saved_rds/sissimdata3_0813.RData",designed_params=data.frame(X1=4,X2=4,X3=4,X4=-6*sqrt(2)),name = "sis_scenario3_revisionv2")
