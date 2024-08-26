
# this file is to get figure 4

library(ggplot2)

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

param3highdimsmall=get_figures(beta=c(2,-3,3,-2,-3,3,2),colnames_dt=c("X1","X2","X3","X4","X5","X6","X1000"),given_names = c("X1","X2","X3","X4","X5","X6","X1000"),scenario1="saved_rds/simdata_highdim_param3_0308.RData",scenario2 = "saved_rds/simdata_highdim_outlier0.001_param3_0308.RData",scenario3 = "saved_rds/simdata_highdim_outlier0.002_param3_0308.RData",scenario4 = "saved_rds/simdata_highdim_outlier0.005_param3_0308.RData",scenario5 = "saved_rds/simdata_highdim_outlier0.01_param3_0308.RData",scenario6 = "saved_rds/simdata_highdim_outlier0.02_param3_0308.RData",scenario7 = "saved_rds/simdata_highdim_outlier0.05_param3_0308.RData",scenario8 = "saved_rds/simdata_highdim_outlier0.1_param3_0308.RData",scenario9 = "saved_rds/simdata_highdim_outlier0.2_param3_0308.RData",designed_params=data.frame(X1=2,X2=-3,X3=3,X4=-2,X5=-3,X6=3,X1000=2),name="Param3_highdim")

param3highdimlarge=get_figures(beta=c(2,-3,3,-2,-3,3,2),colnames_dt=c("X1","X2","X3","X4","X5","X6","X1000"),given_names = c("X1","X2","X3","X4","X5","X6","X1000"),scenario1="saved_rds/simdata_highdim_param3_0311.RData",scenario2 = "saved_rds/simdata_highdim_outlier0.001_param3_0311.RData",scenario3 = "saved_rds/simdata_highdim_outlier0.002_param3_0311.RData",scenario4 = "saved_rds/simdata_highdim_outlier0.005_param3_0311.RData",scenario5 = "saved_rds/simdata_highdim_outlier0.01_param3_0311.RData",scenario6 = "saved_rds/simdata_highdim_outlier0.02_param3_0311.RData",scenario7 = "saved_rds/simdata_highdim_outlier0.05_param3_0311.RData",scenario8 = "saved_rds/simdata_highdim_outlier0.1_param3_0311.RData",scenario9 = "saved_rds/simdata_highdim_outlier0.2_param3_0311.RData",designed_params=data.frame(X1=2,X2=-3,X3=3,X4=-2,X5=-3,X6=3,X1000=2),name="Param3_highdim_1000")


#0.1outlier level for mean, sum abs and mse heatmap
gg_heatmap_data <- melt(param3highdimsmall[[7]][[8]], id.vars = c("method"), variable.name = "variables")
ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
  geom_tile(color = "grey")+scale_fill_gradientn(limits = c(-3,3),colours = c("#2166AC", "#67A9CF" ,"#D1E5F0", "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Estimated params')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
  geom_text(aes(label = round(value,2)), color = "black", size = 4)+ggtitle("EST (n=300)")
p1=ggheatmap

gg_heatmap_data <- melt(param3highdimlarge[[7]][[8]], id.vars = c("method"), variable.name = "variables")
ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
  geom_tile(color = "grey")+scale_fill_gradientn(limits = c(-3,3),colours = c("#2166AC", "#67A9CF" ,"#D1E5F0", "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Estimated params')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
  geom_text(aes(label = round(value,2)), color = "black", size = 4)+ggtitle("EST (n=1000)")
p2=ggheatmap

gg_heatmap_data1 <- melt(param3highdimsmall[[7]][[8]], id.vars = c("method"), variable.name = "variables")
gg_heatmap_data2 <- melt(param3highdimlarge[[7]][[8]], id.vars = c("method"), variable.name = "variables")
gg_heatmap_data=gg_heatmap_data1
gg_heatmap_data$value=abs(gg_heatmap_data1$value-gg_heatmap_data2$value)
ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
  geom_tile(color = "grey")+scale_fill_gradientn(limits = c(-3,3),colours = c("#2166AC", "#67A9CF" ,"#D1E5F0", "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Estimated params')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
  geom_text(aes(label = round(value,2)), color = "black", size = 4)+ggtitle("EST (n=300)-EST (n=1000)")
p3=ggheatmap


param3highdimsmall=get_figures(beta=c(2,-3,3,-2,-3,3,2),colnames_dt=c("X1","X2","X3","X4","X5","X6","X7"),given_names = c("X1","X2","X3","X4","X5","X6","X7"),scenario1="saved_rds/simdata_highdim_param3_0308.RData",scenario2 = "saved_rds/simdata_highdim_outlier0.001_param3_0308.RData",scenario3 = "saved_rds/simdata_highdim_outlier0.002_param3_0308.RData",scenario4 = "saved_rds/simdata_highdim_outlier0.005_param3_0308.RData",scenario5 = "saved_rds/simdata_highdim_outlier0.01_param3_0308.RData",scenario6 = "saved_rds/simdata_highdim_outlier0.02_param3_0308.RData",scenario7 = "saved_rds/simdata_highdim_outlier0.05_param3_0308.RData",scenario8 = "saved_rds/simdata_highdim_outlier0.1_param3_0308.RData",scenario9 = "saved_rds/simdata_highdim_outlier0.2_param3_0308.RData",designed_params=data.frame(X1=2,X2=-3,X3=3,X4=-2,X5=-3,X6=3,X1000=2),name="Param3_highdim")

param3highdimlarge=get_figures(beta=c(2,-3,3,-2,-3,3,2),colnames_dt=c("X1","X2","X3","X4","X5","X6","X7"),given_names = c("X1","X2","X3","X4","X5","X6","X7"),scenario1="saved_rds/simdata_highdim_param3_0311.RData",scenario2 = "saved_rds/simdata_highdim_outlier0.001_param3_0311.RData",scenario3 = "saved_rds/simdata_highdim_outlier0.002_param3_0311.RData",scenario4 = "saved_rds/simdata_highdim_outlier0.005_param3_0311.RData",scenario5 = "saved_rds/simdata_highdim_outlier0.01_param3_0311.RData",scenario6 = "saved_rds/simdata_highdim_outlier0.02_param3_0311.RData",scenario7 = "saved_rds/simdata_highdim_outlier0.05_param3_0311.RData",scenario8 = "saved_rds/simdata_highdim_outlier0.1_param3_0311.RData",scenario9 = "saved_rds/simdata_highdim_outlier0.2_param3_0311.RData",designed_params=data.frame(X1=2,X2=-3,X3=3,X4=-2,X5=-3,X6=3,X1000=2),name="Param3_highdim_1000")

dt1=param3highdimsmall[[8]][[8]]
colnames(dt1)[7]="X1000"
gg_heatmap_data <- melt(dt1, id.vars = c("method"), variable.name = "variables")
ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
  geom_tile(color = "grey")+scale_fill_gradientn(limits = c(0,3),colours = c( "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Absolute value ')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
  geom_text(aes(label = round(value,2)), color = "black", size = 4)+ggtitle("ABS (n=300)")
p4=ggheatmap

dt2=param3highdimlarge[[8]][[8]]
colnames(dt2)[7]="X1000"
gg_heatmap_data <- melt(dt2, id.vars = c("method"), variable.name = "variables")
ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
  geom_tile(color = "grey")+scale_fill_gradientn(limits = c(0,3),colours = c( "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Absolute value')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
  geom_text(aes(label = round(value,2)), color = "black", size = 4)+ggtitle("ABS (n=1000)")
p5=ggheatmap


gg_heatmap_data1 <- melt(dt1, id.vars = c("method"), variable.name = "variables")
gg_heatmap_data2 <- melt(dt2, id.vars = c("method"), variable.name = "variables")
gg_heatmap_data=gg_heatmap_data1
gg_heatmap_data$value=abs(gg_heatmap_data1$value-gg_heatmap_data2$value)
ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
  geom_tile(color = "grey")+scale_fill_gradientn(limits = c(0,3),colours = c( "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'Absolute value')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
  geom_text(aes(label = round(value,2)), color = "black", size = 4)+ggtitle("ABS (n=300)-ABS (n=1000)")
p6=ggheatmap

dt1=param3highdimsmall[[9]][[8]]
colnames(dt1)[7]="X1000"
gg_heatmap_data <- melt(dt1, id.vars = c("method"), variable.name = "variables")
ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
  geom_tile(color = "grey")+scale_fill_gradientn(limits = c(0,10),colours = c( "#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'MSE')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
  geom_text(aes(label = round(value,2)), color = "black", size = 4)+ggtitle("MSE (n=300)")
p7=ggheatmap

dt2=param3highdimlarge[[9]][[8]]
colnames(dt2)[7]="X1000"
gg_heatmap_data <- melt(dt2, id.vars = c("method"), variable.name = "variables")
ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
  geom_tile(color = "grey")+scale_fill_gradientn(limits = c(0,10),colours = c("#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'MSE')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
  geom_text(aes(label = round(value,2)), color = "black", size = 4)+ggtitle("MSE (n=1000)")
p8=ggheatmap

gg_heatmap_data1 <- melt(dt1, id.vars = c("method"), variable.name = "variables")
gg_heatmap_data2 <- melt(dt2, id.vars = c("method"), variable.name = "variables")
gg_heatmap_data=gg_heatmap_data1
gg_heatmap_data$value=abs(gg_heatmap_data1$value-gg_heatmap_data2$value)
ggheatmap <-ggplot(gg_heatmap_data, aes(variables, method, fill= value)) + 
  geom_tile(color = "grey")+scale_fill_gradientn(limits = c(0,10),colours = c("#FFFFFF","#FDDBC7", "#EF8A62", "#B2182B"))+ theme(aspect.ratio = 1, text = element_text(size = 15), legend.position = "bottom") + labs(y= 'Method', x = "Variables", fill = 'MSE')+theme_bw()+theme(axis.text.x = element_text(color = "black", size = 15,angle = 90),axis.text.y = element_text(color = "black", size = 16))+
  geom_text(aes(label = round(value,2)), color = "black", size = 4)+ggtitle("MSE (n=300)-MSE (n=1000)")
p9=ggheatmap

library(gridExtra)
gg=grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol = 3) 
ggsave(file="figure4.pdf",gg,height = 12,width=15)
