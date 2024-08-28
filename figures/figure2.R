
# this is to output figure 2

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

get_figures=function(beta, colnames_dt, given_names, scenario1, scenario2,scenario3,scenario4,scenario5,scenario6,scenario7,scenario8,scenario9,designed_params,name)
{
  load(scenario1)
  # dd=as.data.frame(matrix(0,nrow=6, ncol=7))
  # dd[1,]=beta
  # colnames(dd)=colnames_dt
  # all_list=list(beta,summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  # 
  #   for(i in 2:length(all_list)){
  #     for(j in 1:length(beta)){
  #       dd[i,j]=ifelse(colnames(dd)[j]%in%names(all_list[[i]][[1]]), all_list[[i]][[1]][which(names(all_list[[i]][[1]])==colnames(dd)[j])],0)
  #     }
  #   }
  #  
  # dd2=dd
  # dd2=sweep(dd2,2,beta)
  # dd2=dd2[-1,]
  # dd2$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  # highdim_param1=dd2
  
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun2(result3,given_names),calculation_fun2(result4,given_names),calculation_fun2(result5,given_names))
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
  
  highdim_param1_return_table=cbind.data.frame(highdim_param1_tp,highdim_param1_fp,highdim_param1_f1,highdim_param1_correctprob,highdim_param1_medianmodelsize)
  colnames(highdim_param1_return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  highdim_param1_return_table$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  highdim_param1_mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    highdim_param1_mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
  }
  colnames(highdim_param1_mean_est)=given_names
  highdim_param1_mean_est$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  highdim_param1=sweep(highdim_param1_mean_est[,1:7],2,beta)
  highdim_param1$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_sum_abs=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    highdim_param1_sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
  }
  colnames(highdim_param1_sum_abs)=given_names
  highdim_param1_sum_abs$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_mse=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    highdim_param1_mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
  }
  colnames(highdim_param1_mse)=given_names
  highdim_param1_mse$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  #     #
  load(scenario2)
  # dd=as.data.frame(matrix(0,nrow=6, ncol=7))
  # dd[1,]=beta
  # colnames(dd)=colnames_dt
  # all_list=list(beta,summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  # 
  #   for(i in 2:length(all_list)){
  #     for(j in 1:length(beta)){
  #       dd[i,j]=ifelse(colnames(dd)[j]%in%names(all_list[[i]][[1]]), all_list[[i]][[1]][which(names(all_list[[i]][[1]])==colnames(dd)[j])],0)
  #     }
  #   }
  #  
  # dd2=dd
  # dd2=sweep(dd2,2,beta)
  # dd2=dd2[-1,]
  # dd2$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  # highdim_param1_outlier0.001=dd2
  
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun2(result3,given_names),calculation_fun2(result4,given_names),calculation_fun2(result5,given_names))
  final_cal_table=as.data.frame(matrix(0,nrow=length(all_cal_list),ncol=3))
  for(i in 1:length(all_cal_list)){
    for(j in 1:3){
      final_cal_table[i,j]=all_cal_list[[i]][[1]][[j]]
    }
  }
  highdim_param1_outlier0.001_f1=final_cal_table$V3
  highdim_param1_outlier0.001_tp=final_cal_table$V1
  highdim_param1_outlier0.001_fp=final_cal_table$V2
  
  highdim_param1_outlier0.001_correctprob=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.001_correctprob[i]=all_cal_list[[i]][[4]]/100
  }
  
  highdim_param1_outlier0.001_medianmodelsize=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.001_medianmodelsize[i]=median(all_cal_list[[i]][[3]])
  }
  
  highdim_param1_outlier0.001_return_table=cbind.data.frame(highdim_param1_outlier0.001_tp,highdim_param1_outlier0.001_fp,highdim_param1_outlier0.001_f1,highdim_param1_outlier0.001_correctprob,highdim_param1_outlier0.001_medianmodelsize)
  colnames(highdim_param1_outlier0.001_return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  highdim_param1_outlier0.001_return_table$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  highdim_param1_outlier0.001_mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    highdim_param1_outlier0.001_mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
  }
  colnames(highdim_param1_outlier0.001_mean_est)=given_names
  highdim_param1_outlier0.001_mean_est$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  highdim_param1_outlier0.001=sweep(highdim_param1_outlier0.001_mean_est[,1:7],2,beta)
  highdim_param1_outlier0.001$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.001_sum_abs=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.001_sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
    
  }
  colnames(highdim_param1_outlier0.001_sum_abs)=given_names
  highdim_param1_outlier0.001_sum_abs$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.001_mse=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.001_mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
    
  }
  colnames(highdim_param1_outlier0.001_mse)=given_names
  highdim_param1_outlier0.001_mse$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  #
  load(scenario3)
  # dd=as.data.frame(matrix(0,nrow=6, ncol=7))
  # dd[1,]=beta
  # colnames(dd)=colnames_dt
  # all_list=list(beta,summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  # 
  #   for(i in 2:length(all_list)){
  #     for(j in 1:length(beta)){
  #       dd[i,j]=ifelse(colnames(dd)[j]%in%names(all_list[[i]][[1]]), all_list[[i]][[1]][which(names(all_list[[i]][[1]])==colnames(dd)[j])],0)
  #     }
  #   }
  #  
  # dd2=dd
  # dd2=sweep(dd2,2,beta)
  # dd2=dd2[-1,]
  # dd2$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  # highdim_param1_outlier0.002=dd2
  
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun2(result3,given_names),calculation_fun2(result4,given_names),calculation_fun2(result5,given_names))
  final_cal_table=as.data.frame(matrix(0,nrow=length(all_cal_list),ncol=3))
  for(i in 1:length(all_cal_list)){
    for(j in 1:3){
      final_cal_table[i,j]=all_cal_list[[i]][[1]][[j]]
    }
  }
  highdim_param1_outlier0.002_f1=final_cal_table$V3
  highdim_param1_outlier0.002_tp=final_cal_table$V1
  highdim_param1_outlier0.002_fp=final_cal_table$V2
  
  highdim_param1_outlier0.002_correctprob=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.002_correctprob[i]=all_cal_list[[i]][[4]]/100
  }
  
  highdim_param1_outlier0.002_medianmodelsize=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.002_medianmodelsize[i]=median(all_cal_list[[i]][[3]])
  }
  
  highdim_param1_outlier0.002_return_table=cbind.data.frame(highdim_param1_outlier0.002_tp,highdim_param1_outlier0.002_fp,highdim_param1_outlier0.002_f1,highdim_param1_outlier0.002_correctprob,highdim_param1_outlier0.002_medianmodelsize)
  colnames(highdim_param1_outlier0.002_return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  highdim_param1_outlier0.002_return_table$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  highdim_param1_outlier0.002_mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.002_mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
    
  }
  colnames(highdim_param1_outlier0.002_mean_est)=given_names
  highdim_param1_outlier0.002_mean_est$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  highdim_param1_outlier0.002=sweep(highdim_param1_outlier0.002_mean_est[,1:7],2,beta)
  highdim_param1_outlier0.002$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.002_sum_abs=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.002_sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
    
  }
  colnames(highdim_param1_outlier0.002_sum_abs)=given_names
  highdim_param1_outlier0.002_sum_abs$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.002_mse=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.002_mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
    
  }
  colnames(highdim_param1_outlier0.002_mse)=given_names
  highdim_param1_outlier0.002_mse$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  #
  #
  load(scenario4)
  # dd=as.data.frame(matrix(0,nrow=6, ncol=7))
  # dd[1,]=beta
  # colnames(dd)=colnames_dt
  # all_list=list(beta,summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  # 
  #   for(i in 2:length(all_list)){
  #     for(j in 1:length(beta)){
  #       dd[i,j]=ifelse(colnames(dd)[j]%in%names(all_list[[i]][[1]]), all_list[[i]][[1]][which(names(all_list[[i]][[1]])==colnames(dd)[j])],0)
  #     }
  #   }
  #  
  # dd2=dd
  # dd2=sweep(dd2,2,beta)
  # dd2=dd2[-1,]
  # dd2$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  # highdim_param1_outlier0.005=dd2
  
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun2(result3,given_names),calculation_fun2(result4,given_names),calculation_fun2(result5,given_names))
  final_cal_table=as.data.frame(matrix(0,nrow=length(all_cal_list),ncol=3))
  for(i in 1:length(all_cal_list)){
    for(j in 1:3){
      final_cal_table[i,j]=all_cal_list[[i]][[1]][[j]]
    }
  }
  highdim_param1_outlier0.005_f1=final_cal_table$V3
  highdim_param1_outlier0.005_tp=final_cal_table$V1
  highdim_param1_outlier0.005_fp=final_cal_table$V2
  
  highdim_param1_outlier0.005_correctprob=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.005_correctprob[i]=all_cal_list[[i]][[4]]/100
  }
  
  highdim_param1_outlier0.005_medianmodelsize=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.005_medianmodelsize[i]=median(all_cal_list[[i]][[3]])
  }
  
  highdim_param1_outlier0.005_return_table=cbind.data.frame(highdim_param1_outlier0.005_tp,highdim_param1_outlier0.005_fp,highdim_param1_outlier0.005_f1,highdim_param1_outlier0.005_correctprob,highdim_param1_outlier0.005_medianmodelsize)
  colnames(highdim_param1_outlier0.005_return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  highdim_param1_outlier0.005_return_table$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  highdim_param1_outlier0.005_mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.005_mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
    
  }
  colnames(highdim_param1_outlier0.005_mean_est)=given_names
  highdim_param1_outlier0.005_mean_est$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  highdim_param1_outlier0.005=sweep(highdim_param1_outlier0.005_mean_est[,1:7],2,beta)
  highdim_param1_outlier0.005$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.005_sum_abs=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.005_sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
    
  }
  colnames(highdim_param1_outlier0.005_sum_abs)=given_names
  highdim_param1_outlier0.005_sum_abs$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.005_mse=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.005_mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
    
  }
  colnames(highdim_param1_outlier0.005_mse)=given_names
  highdim_param1_outlier0.005_mse$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  #
  load(scenario5)
  # dd=as.data.frame(matrix(0,nrow=6, ncol=7))
  # dd[1,]=beta
  # colnames(dd)=colnames_dt
  # all_list=list(beta,summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  # 
  #   for(i in 2:length(all_list)){
  #     for(j in 1:length(beta)){
  #       dd[i,j]=ifelse(colnames(dd)[j]%in%names(all_list[[i]][[1]]), all_list[[i]][[1]][which(names(all_list[[i]][[1]])==colnames(dd)[j])],0)
  #     }
  #   }
  #  
  # dd2=dd
  # dd2=sweep(dd2,2,beta)
  # dd2=dd2[-1,]
  # dd2$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  # highdim_param1_outlier0.01=dd2
  
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun2(result3,given_names),calculation_fun2(result4,given_names),calculation_fun2(result5,given_names))
  final_cal_table=as.data.frame(matrix(0,nrow=length(all_cal_list),ncol=3))
  for(i in 1:length(all_cal_list)){
    for(j in 1:3){
      final_cal_table[i,j]=all_cal_list[[i]][[1]][[j]]
    }
  }
  highdim_param1_outlier0.01_f1=final_cal_table$V3
  highdim_param1_outlier0.01_tp=final_cal_table$V1
  highdim_param1_outlier0.01_fp=final_cal_table$V2
  
  highdim_param1_outlier0.01_correctprob=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.01_correctprob[i]=all_cal_list[[i]][[4]]/100
  }
  
  highdim_param1_outlier0.01_medianmodelsize=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.01_medianmodelsize[i]=median(all_cal_list[[i]][[3]])
  }
  
  highdim_param1_outlier0.01_return_table=cbind.data.frame(highdim_param1_outlier0.01_tp,highdim_param1_outlier0.01_fp,highdim_param1_outlier0.01_f1,highdim_param1_outlier0.01_correctprob,highdim_param1_outlier0.01_medianmodelsize)
  colnames(highdim_param1_outlier0.01_return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  highdim_param1_outlier0.01_return_table$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  highdim_param1_outlier0.01_mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.01_mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
    
  }
  colnames(highdim_param1_outlier0.01_mean_est)=given_names
  highdim_param1_outlier0.01_mean_est$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  highdim_param1_outlier0.01=sweep(highdim_param1_outlier0.01_mean_est[,1:7],2,beta)
  highdim_param1_outlier0.01$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.01_sum_abs=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.01_sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
    
  }
  colnames(highdim_param1_outlier0.01_sum_abs)=given_names
  highdim_param1_outlier0.01_sum_abs$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.01_mse=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.01_mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
    
  }
  colnames(highdim_param1_outlier0.01_mse)=given_names
  highdim_param1_outlier0.01_mse$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  #
  load(scenario6)
  # dd=as.data.frame(matrix(0,nrow=6, ncol=7))
  # dd[1,]=beta
  # colnames(dd)=colnames_dt
  # all_list=list(beta,summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  # 
  #   for(i in 2:length(all_list)){
  #     for(j in 1:length(beta)){
  #       dd[i,j]=ifelse(colnames(dd)[j]%in%names(all_list[[i]][[1]]), all_list[[i]][[1]][which(names(all_list[[i]][[1]])==colnames(dd)[j])],0)
  #     }
  #   }
  #  
  # dd2=dd
  # dd2=sweep(dd2,2,beta)
  # dd2=dd2[-1,]
  # dd2$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  # highdim_param1_outlier0.02=dd2
  
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun2(result3,given_names),calculation_fun2(result4,given_names),calculation_fun2(result5,given_names))
  final_cal_table=as.data.frame(matrix(0,nrow=length(all_cal_list),ncol=3))
  for(i in 1:length(all_cal_list)){
    for(j in 1:3){
      final_cal_table[i,j]=all_cal_list[[i]][[1]][[j]]
    }
  }
  highdim_param1_outlier0.02_f1=final_cal_table$V3
  highdim_param1_outlier0.02_tp=final_cal_table$V1
  highdim_param1_outlier0.02_fp=final_cal_table$V2
  
  highdim_param1_outlier0.02_correctprob=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.02_correctprob[i]=all_cal_list[[i]][[4]]/100
  }
  
  highdim_param1_outlier0.02_medianmodelsize=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.02_medianmodelsize[i]=median(all_cal_list[[i]][[3]])
  }
  
  highdim_param1_outlier0.02_return_table=cbind.data.frame(highdim_param1_outlier0.02_tp,highdim_param1_outlier0.02_fp,highdim_param1_outlier0.02_f1,highdim_param1_outlier0.02_correctprob,highdim_param1_outlier0.02_medianmodelsize)
  colnames(highdim_param1_outlier0.02_return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  highdim_param1_outlier0.02_return_table$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  highdim_param1_outlier0.02_mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.02_mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
    
  }
  colnames(highdim_param1_outlier0.02_mean_est)=given_names
  highdim_param1_outlier0.02_mean_est$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  highdim_param1_outlier0.02=sweep(highdim_param1_outlier0.02_mean_est[,1:7],2,beta)
  highdim_param1_outlier0.02$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.02_sum_abs=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.02_sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
    
  }
  colnames(highdim_param1_outlier0.02_sum_abs)=given_names
  highdim_param1_outlier0.02_sum_abs$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.02_mse=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.02_mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
    
  }
  colnames(highdim_param1_outlier0.02_mse)=given_names
  highdim_param1_outlier0.02_mse$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  #
  load(scenario7)
  # dd=as.data.frame(matrix(0,nrow=6, ncol=7))
  # dd[1,]=beta
  # colnames(dd)=colnames_dt
  # all_list=list(beta,summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  # 
  #   for(i in 2:length(all_list)){
  #     for(j in 1:length(beta)){
  #       dd[i,j]=ifelse(colnames(dd)[j]%in%names(all_list[[i]][[1]]), all_list[[i]][[1]][which(names(all_list[[i]][[1]])==colnames(dd)[j])],0)
  #     }
  #   }
  #  
  # dd2=dd
  # dd2=sweep(dd2,2,beta)
  # dd2=dd2[-1,]
  # dd2$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  # highdim_param1_outlier0.05=dd2
  
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun2(result3,given_names),calculation_fun2(result4,given_names),calculation_fun2(result5,given_names))
  final_cal_table=as.data.frame(matrix(0,nrow=length(all_cal_list),ncol=3))
  for(i in 1:length(all_cal_list)){
    for(j in 1:3){
      final_cal_table[i,j]=all_cal_list[[i]][[1]][[j]]
    }
  }
  highdim_param1_outlier0.05_f1=final_cal_table$V3
  highdim_param1_outlier0.05_tp=final_cal_table$V1
  highdim_param1_outlier0.05_fp=final_cal_table$V2
  
  highdim_param1_outlier0.05_correctprob=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.05_correctprob[i]=all_cal_list[[i]][[4]]/100
  }
  
  highdim_param1_outlier0.05_medianmodelsize=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.05_medianmodelsize[i]=median(all_cal_list[[i]][[3]])
  }
  
  highdim_param1_outlier0.05_return_table=cbind.data.frame(highdim_param1_outlier0.05_tp,highdim_param1_outlier0.05_fp,highdim_param1_outlier0.05_f1,highdim_param1_outlier0.05_correctprob,highdim_param1_outlier0.05_medianmodelsize)
  colnames(highdim_param1_outlier0.05_return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  highdim_param1_outlier0.05_return_table$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  highdim_param1_outlier0.05_mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.05_mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
    
  }
  colnames(highdim_param1_outlier0.05_mean_est)=given_names
  highdim_param1_outlier0.05_mean_est$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  highdim_param1_outlier0.05=sweep(highdim_param1_outlier0.05_mean_est[,1:7],2,beta)
  highdim_param1_outlier0.05$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.05_sum_abs=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.05_sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
    
  }
  colnames(highdim_param1_outlier0.05_sum_abs)=given_names
  highdim_param1_outlier0.05_sum_abs$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.05_mse=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.05_mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
    
  }
  colnames(highdim_param1_outlier0.05_mse)=given_names
  highdim_param1_outlier0.05_mse$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  #
  load(scenario8)
  # dd=as.data.frame(matrix(0,nrow=6, ncol=7))
  # dd[1,]=beta
  # colnames(dd)=colnames_dt
  # all_list=list(beta,summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  # 
  #   for(i in 2:length(all_list)){
  #     for(j in 1:length(beta)){
  #       dd[i,j]=ifelse(colnames(dd)[j]%in%names(all_list[[i]][[1]]), all_list[[i]][[1]][which(names(all_list[[i]][[1]])==colnames(dd)[j])],0)
  #     }
  #   }
  
  # dd2=dd
  # dd2=sweep(dd2,2,beta)
  # dd2=dd2[-1,]
  # dd2$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  # highdim_param1_outlier0.1=dd2
  
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun2(result3,given_names),calculation_fun2(result4,given_names),calculation_fun2(result5,given_names))
  final_cal_table=as.data.frame(matrix(0,nrow=length(all_cal_list),ncol=3))
  for(i in 1:length(all_cal_list)){
    for(j in 1:3){
      final_cal_table[i,j]=all_cal_list[[i]][[1]][[j]]
    }
  }
  highdim_param1_outlier0.1_f1=final_cal_table$V3
  highdim_param1_outlier0.1_tp=final_cal_table$V1
  highdim_param1_outlier0.1_fp=final_cal_table$V2
  
  highdim_param1_outlier0.1_correctprob=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.1_correctprob[i]=all_cal_list[[i]][[4]]/100
  }
  
  highdim_param1_outlier0.1_medianmodelsize=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.1_medianmodelsize[i]=median(all_cal_list[[i]][[3]])
  }
  
  highdim_param1_outlier0.1_return_table=cbind.data.frame(highdim_param1_outlier0.1_tp,highdim_param1_outlier0.1_fp,highdim_param1_outlier0.1_f1,highdim_param1_outlier0.1_correctprob,highdim_param1_outlier0.1_medianmodelsize)
  colnames(highdim_param1_outlier0.1_return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  highdim_param1_outlier0.1_return_table$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  highdim_param1_outlier0.1_mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.1_mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
    
  }
  colnames(highdim_param1_outlier0.1_mean_est)=given_names
  highdim_param1_outlier0.1_mean_est$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  highdim_param1_outlier0.1=sweep(highdim_param1_outlier0.1_mean_est[,1:7],2,beta)
  highdim_param1_outlier0.1$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.1_sum_abs=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.1_sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
    
  }
  colnames(highdim_param1_outlier0.1_sum_abs)=given_names
  highdim_param1_outlier0.1_sum_abs$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.1_mse=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.1_mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
    
  }
  colnames(highdim_param1_outlier0.1_mse)=given_names
  highdim_param1_outlier0.1_mse$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  #
  load(scenario9)
  # dd=as.data.frame(matrix(0,nrow=6, ncol=7))
  # dd[1,]=beta
  # colnames(dd)=colnames_dt
  # all_list=list(beta,summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  # 
  #   for(i in 2:length(all_list)){
  #     for(j in 1:length(beta)){
  #       dd[i,j]=ifelse(colnames(dd)[j]%in%names(all_list[[i]][[1]]), all_list[[i]][[1]][which(names(all_list[[i]][[1]])==colnames(dd)[j])],0)
  #     }
  #   }
  #  
  # dd2=dd
  # dd2=sweep(dd2,2,beta)
  # dd2=dd2[-1,]
  # dd2$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  # highdim_param1_outlier0.2=dd2
  
  all_cal_list=list(calculation_fun(result1,given_names),calculation_fun(result2,given_names),calculation_fun2(result3,given_names),calculation_fun2(result4,given_names),calculation_fun2(result5,given_names))
  final_cal_table=as.data.frame(matrix(0,nrow=length(all_cal_list),ncol=3))
  for(i in 1:length(all_cal_list)){
    for(j in 1:3){
      final_cal_table[i,j]=all_cal_list[[i]][[1]][[j]]
    }
  }
  highdim_param1_outlier0.2_f1=final_cal_table$V3
  highdim_param1_outlier0.2_tp=final_cal_table$V1
  highdim_param1_outlier0.2_fp=final_cal_table$V2
  
  highdim_param1_outlier0.2_correctprob=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.2_correctprob[i]=all_cal_list[[i]][[4]]/100
  }
  
  highdim_param1_outlier0.2_medianmodelsize=c()
  for(i in 1:length(all_cal_list)){
    highdim_param1_outlier0.2_medianmodelsize[i]=median(all_cal_list[[i]][[3]])
  }
  
  highdim_param1_outlier0.2_return_table=cbind.data.frame(highdim_param1_outlier0.2_tp,highdim_param1_outlier0.2_fp,highdim_param1_outlier0.2_f1,highdim_param1_outlier0.2_correctprob,highdim_param1_outlier0.2_medianmodelsize)
  colnames(highdim_param1_outlier0.2_return_table)=c("TP","FP","F1 score","Correct mod prob","Median mod size")
  highdim_param1_outlier0.2_return_table$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  all_list2=list(summary_table_fun(result1,designed_params),summary_table_fun(result2,designed_params),summary_table_fun2(result3,designed_params),summary_table_fun2(result4,designed_params),summary_table_fun2(result5,designed_params))
  highdim_param1_outlier0.2_mean_est=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.2_mean_est[i,which(given_names%in%names(all_list2[[i]][[2]]))]=all_list2[[i]][[2]]
    
  }
  colnames(highdim_param1_outlier0.2_mean_est)=given_names
  highdim_param1_outlier0.2_mean_est$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  highdim_param1_outlier0.2=sweep(highdim_param1_outlier0.2_mean_est[,1:7],2,beta)
  highdim_param1_outlier0.2$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.2_sum_abs=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.2_sum_abs[i,which(given_names%in%names(all_list2[[i]][[3]]))]=all_list2[[i]][[3]]
    
  }
  colnames(highdim_param1_outlier0.2_sum_abs)=given_names
  highdim_param1_outlier0.2_sum_abs$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  highdim_param1_outlier0.2_mse=data.frame(matrix(0,nrow = length(all_list2),ncol = 7))
  for(i in 1:length(all_list2)){
    
    highdim_param1_outlier0.2_mse[i,which(given_names%in%names(all_list2[[i]][[4]]))]=all_list2[[i]][[4]]
    
  }
  colnames(highdim_param1_outlier0.2_mse)=given_names
  highdim_param1_outlier0.2_mse$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  
  ########
  highdim_param1_all=rbind.data.frame(highdim_param1,highdim_param1_outlier0.001,highdim_param1_outlier0.002,highdim_param1_outlier0.005,highdim_param1_outlier0.01,highdim_param1_outlier0.02,highdim_param1_outlier0.05,highdim_param1_outlier0.1,highdim_param1_outlier0.2)
  highdim_param1_all$outlier=c(rep(0,5),rep(0.001,5),rep(0.002,5),rep(0.005,5),rep(0.01,5),rep(0.02,5),rep(0.05,5),rep(0.1,5),rep(0.2,5))
  highdim_param1_all$outlier_level=c(rep("0",5),rep("0.001",5),rep("0.002",5),rep("0.005",5),rep("0.01",5),rep("0.02",5),rep("0.05",5),rep("0.1",5),rep("0.2",5))
  highdim_param1_all$method=factor(highdim_param1_all$method,levels = c("lasso","enet","mcp","pawph","sis"))
  highdim_param1_long <- melt(setDT(highdim_param1_all), id.vars = c("method","outlier","outlier_level"), variable.name = "Variable")
  
  
  p <- ggplot(data = highdim_param1_long, aes(x = outlier_level, y = value,color=method,group=method)) + geom_line()+geom_point()+theme_bw()+ggtitle("Comparison with different levels of outliers (show diff in est.)")+geom_hline(yintercept = 0,color="black")
  p + facet_wrap(~Variable,ncol=7)
  #ggsave(p + facet_wrap(~Variable,ncol=7),file=paste(name,"line_plot.pdf",sep = "_"),height=4,width=24)
  
  
  highdim_param1_f1_all=cbind.data.frame(highdim_param1_f1,highdim_param1_outlier0.001_f1,highdim_param1_outlier0.002_f1,highdim_param1_outlier0.005_f1,highdim_param1_outlier0.01_f1,highdim_param1_outlier0.02_f1,highdim_param1_outlier0.05_f1,highdim_param1_outlier0.1_f1,highdim_param1_outlier0.2_f1)
  highdim_param1_f1_all$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  colnames(highdim_param1_f1_all)=c("0","0.001","0.002","0.005","0.01","0.02","0.05","0.1","0.2","Method")
  
  highdim_param1_tp_all=cbind.data.frame(highdim_param1_tp,highdim_param1_outlier0.001_tp,highdim_param1_outlier0.002_tp,highdim_param1_outlier0.005_tp,highdim_param1_outlier0.01_tp,highdim_param1_outlier0.02_tp,highdim_param1_outlier0.05_tp,highdim_param1_outlier0.1_tp,highdim_param1_outlier0.2_tp)
  highdim_param1_tp_all$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  colnames(highdim_param1_tp_all)=c("0","0.001","0.002","0.005","0.01","0.02","0.05","0.1","0.2","Method")
  
  highdim_param1_fp_all=cbind.data.frame(highdim_param1_fp,highdim_param1_outlier0.001_fp,highdim_param1_outlier0.002_fp,highdim_param1_outlier0.005_fp,highdim_param1_outlier0.01_fp,highdim_param1_outlier0.02_fp,highdim_param1_outlier0.05_fp,highdim_param1_outlier0.1_fp,highdim_param1_outlier0.2_fp)
  highdim_param1_fp_all$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  colnames(highdim_param1_fp_all)=c("0","0.001","0.002","0.005","0.01","0.02","0.05","0.1","0.2","Method")
  
  highdim_param1_medianmodelsize_all=cbind.data.frame(highdim_param1_medianmodelsize,highdim_param1_outlier0.001_medianmodelsize,highdim_param1_outlier0.002_medianmodelsize,highdim_param1_outlier0.005_medianmodelsize,highdim_param1_outlier0.01_medianmodelsize,highdim_param1_outlier0.02_medianmodelsize,highdim_param1_outlier0.05_medianmodelsize,highdim_param1_outlier0.1_medianmodelsize,highdim_param1_outlier0.2_medianmodelsize)
  highdim_param1_medianmodelsize_all$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  colnames(highdim_param1_medianmodelsize_all)=c("0","0.001","0.002","0.005","0.01","0.02","0.05","0.1","0.2","Method")
  
  
  highdim_param1_correctprob_all=cbind.data.frame(highdim_param1_correctprob,highdim_param1_outlier0.001_correctprob,highdim_param1_outlier0.002_correctprob,highdim_param1_outlier0.005_correctprob,highdim_param1_outlier0.01_correctprob,highdim_param1_outlier0.02_correctprob,highdim_param1_outlier0.05_correctprob,highdim_param1_outlier0.1_correctprob,highdim_param1_outlier0.2_correctprob)
  highdim_param1_correctprob_all$method=factor(c("lasso","enet","mcp","pawph","sis"),levels = c("lasso","enet","mcp","pawph","sis"))
  colnames(highdim_param1_correctprob_all)=c("0","0.001","0.002","0.005","0.01","0.02","0.05","0.1","0.2","Method")
  
  highdim_param1_tp_all_long <- melt(setDT(highdim_param1_tp_all), id.vars = c("Method"), variable.name = "Outlier_percentage")
  tpgg=ggplot(data = highdim_param1_tp_all_long,aes(x = Outlier_percentage,y = value,fill=Method)) +geom_bar(stat= "identity",position="dodge")+theme_bw()+ggtitle("True positive")+geom_hline(yintercept=7,color="black")
  #+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #ggsave(tpgg,file=paste(name,"tp_plot.pdf",sep = "_"),height=5,width=10)
  
  highdim_param1_fp_all_long <- melt(setDT(highdim_param1_fp_all), id.vars = c("Method"), variable.name = "Outlier_percentage")
  tpgg=ggplot(data = highdim_param1_fp_all_long,aes(x = Outlier_percentage,y = value,fill=Method)) +geom_bar(stat= "identity",position="dodge")+theme_bw()+ggtitle("False positive")
  #+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #ggsave(tpgg,file=paste(name,"fp_plot.pdf",sep = "_"),height=5,width=10)
  
  highdim_param1_medianmodelsize_all_long <- melt(setDT(highdim_param1_medianmodelsize_all), id.vars = c("Method"), variable.name = "Outlier_percentage")
  tpgg=ggplot(data = highdim_param1_medianmodelsize_all_long,aes(x = Outlier_percentage,y = value,fill=Method)) +geom_bar(stat= "identity",position="dodge")+theme_bw()+ggtitle("Mean model size")
  #+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #ggsave(tpgg,file=paste(name,"mean_model_size_plot.pdf",sep = "_"),height=5,width=10)
  
  highdim_param1_correctprob_all_long <- melt(setDT(highdim_param1_correctprob_all), id.vars = c("Method"), variable.name = "Outlier_percentage")
  tpgg=ggplot(data = highdim_param1_correctprob_all_long,aes(x = Outlier_percentage,y = value,fill=Method)) +geom_bar(stat= "identity",position="dodge")+theme_bw()+ggtitle("Correct prob")+ylim(c(0,1))
  #+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #ggsave(tpgg,file=paste(name,"correctprob_plot.pdf",sep = "_"),height=5,width=10)
  
  
  mean_list=list(highdim_param1_mean_est,highdim_param1_outlier0.001_mean_est,highdim_param1_outlier0.002_mean_est,highdim_param1_outlier0.005_mean_est,highdim_param1_outlier0.01_mean_est,highdim_param1_outlier0.02_mean_est,highdim_param1_outlier0.05_mean_est,highdim_param1_outlier0.1_mean_est,highdim_param1_outlier0.2_mean_est)
  abs_list=list(highdim_param1_sum_abs,highdim_param1_outlier0.001_sum_abs,highdim_param1_outlier0.002_sum_abs,highdim_param1_outlier0.005_sum_abs,highdim_param1_outlier0.01_sum_abs,highdim_param1_outlier0.02_sum_abs,highdim_param1_outlier0.05_sum_abs,highdim_param1_outlier0.1_sum_abs,highdim_param1_outlier0.2_sum_abs)
  mse_list=list(highdim_param1_mse,highdim_param1_outlier0.001_mse,highdim_param1_outlier0.002_mse,highdim_param1_outlier0.005_mse,highdim_param1_outlier0.01_mse,highdim_param1_outlier0.02_mse,highdim_param1_outlier0.05_mse,highdim_param1_outlier0.1_mse,highdim_param1_outlier0.2_mse)
  return(list(highdim_param1_all,highdim_param1_f1_all,highdim_param1_tp_all,highdim_param1_fp_all,highdim_param1_medianmodelsize_all,highdim_param1_correctprob_all,mean_list,abs_list,mse_list))
}


get_figures(beta=c(0,0,3,0,-3,0,5),colnames_dt=c("X1","X2","X3","X4","X5","X6","X1000"),given_names = c("X3","X5","X1000"),scenario1="saved_rds/simdata_highdim_param4_0813.RData",scenario2 = "saved_rds/simdata_highdim_outlier0.001_param4_0813.RData",scenario3 = "saved_rds/simdata_highdim_outlier0.002_param4_0813.RData",scenario4 = "saved_rds/simdata_highdim_outlier0.005_param4_0813.RData",scenario5 = "saved_rds/simdata_highdim_outlier0.01_param4_0813.RData",scenario6 = "saved_rds/simdata_highdim_outlier0.02_param4_0813.RData",scenario7 = "saved_rds/simdata_highdim_outlier0.05_param4_0813.RData",scenario8 = "saved_rds/simdata_highdim_outlier0.1_param4_0813.RData",scenario9 = "saved_rds/simdata_highdim_outlier0.2_param4_0813.RData",designed_params=data.frame(X1=0,X2=0,X3=3,X4=0,X5=-3,X6=0,X1000=5),name="Param4_highdim_revision")
