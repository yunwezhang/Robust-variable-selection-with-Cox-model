

# this r script is to get results (saved .rds files)
# note: this script will take about a week to run on a 4 "NVIDIA RTX A5000" server.

library(Matrix)
library(dplyr)
library(survival)
library(glmnet)
library(ncvreg)
library(ggplot2)
library(caret)
library(Hmisc)
library(SIS)
library(MASS)
library(ncvreg2)
library(faux)
library(ggpubr)
library(hdnom)
library(purrr)
library(reshape2)
library(data.table)
library(gbm)
library(pbmcapply)

result_cal=function(current_data2,model_name){
  X=as.matrix(current_data2[,!colnames(current_data2)%in%c("time","status")])
  y=Surv(current_data2$time,current_data2$status)
  time=current_data2$time
  status=current_data2$status
  if(model_name=="pawph"){
    model6 <- prcoxreg(y,X, seed=1, alpha= 0.5)
    # PAWPH estimator
    result=model6$betaHat_re
    output=result[result!=0]}
  if(model_name=="ncvreg"){
    #ncvreg
    method1<-ncvreg::cv.ncvsurv(X,y,gamma=3,penalty="MCP",alpha=1,nfolds=10)
    result=method1$fit$beta[,which(method1$fit$lambda==method1$lambda.min)]
    output=result[result!=0]
  }
  if(model_name=="SIS"){
    model2=SIS(X,y,family='cox', penalty='lasso', tune='bic', varISIS='cons',seed=41,nfolds = 10,type.measure = "deviance")
    #colnames(X)[model2$ix]
    result=model2$coef.est
    output=result[result!=0]
  }
  if(model_name=="lasso"){
    #lasso
    fitlasso1<-hdnom::fit_lasso(X, Surv(time, status), nfolds = 10, rule = "lambda.min")
    mat=fitlasso1$model$beta
    non_zero_indices <- which(mat[] != 0, arr.ind = TRUE)
    non_zero_values_lasso1 <- data.frame(
      Name = rownames(mat)[non_zero_indices[, 1]],
      Value = mat@x
    )
    output=non_zero_values_lasso1
  }
  if(model_name=="enet"){
    #enet
    fitenet1<-hdnom::fit_enet(X, Surv(time, status), nfolds = 10, rule = "lambda.min")
    mat=fitenet1$model$beta
    non_zero_indices <- which(mat[] != 0, arr.ind = TRUE)
    non_zero_values_enet1 <- data.frame(
      Name = rownames(mat)[non_zero_indices[, 1]],
      Value = mat@x
    )
    output=non_zero_values_enet1
  }
  if(model_name=="alasso"){
    #alasso
    fitlasso1<-hdnom::fit_alasso(X, Surv(time, status), nfolds = 10, rule = "lambda.min")
    mat=fitlasso1$model$beta
    non_zero_indices <- which(mat[] != 0, arr.ind = TRUE)
    non_zero_values_lasso1 <- data.frame(
      Name = rownames(mat)[non_zero_indices[, 1]],
      Value = mat@x
    )
    output=non_zero_values_lasso1
  }
  if(model_name=="aenet"){
    #aenet
    fitenet1<-hdnom::fit_aenet(X, Surv(time, status), nfolds = 10, rule = "lambda.min")
    mat=fitenet1$model$beta
    non_zero_indices <- which(mat[] != 0, arr.ind = TRUE)
    non_zero_values_enet1 <- data.frame(
      Name = rownames(mat)[non_zero_indices[, 1]],
      Value = mat@x
    )
    output=non_zero_values_enet1
  }
  if(model_name=="scad"){
    #alasso
    fitlasso1<-hdnom::fit_scad(X, Surv(time, status), nfolds = 10)
    mat=fitlasso1$model$beta
    non_zero_indices <- which(mat[] != 0, arr.ind = TRUE)
    non_zero_values_lasso1 <- data.frame(
      Name = rownames(mat)[non_zero_indices[, 1]],
      Value = mat[non_zero_indices[, 1],1]
    )
    output=non_zero_values_lasso1
  }
  if(model_name=="mnet"){
    #aenet
    fitenet1<-hdnom::fit_mnet(X, Surv(time, status), nfolds = 10)
    mat=fitenet1$model$beta
    non_zero_indices <- which(mat[] != 0, arr.ind = TRUE)
    non_zero_values_lasso1 <- data.frame(
      Name = rownames(mat)[non_zero_indices[, 1]],
      Value = mat[non_zero_indices[, 1],1]
    )
    output=non_zero_values_lasso1
  }
  if(model_name=="snet"){
    #aenet
    fitenet1<-hdnom::fit_snet(X, Surv(time, status), nfolds = 10)
    mat=fitenet1$model$beta
    non_zero_indices <- which(mat[] != 0, arr.ind = TRUE)
    non_zero_values_lasso1 <- data.frame(
      Name = rownames(mat)[non_zero_indices[, 1]],
      Value = mat[non_zero_indices[, 1],1]
    )
    output=non_zero_values_lasso1
  }
  return(list(output,table(current_data2$status)))
}


my_sim_data_0228=function(seed,n,p,beta,outlier,h0,k.shape,outlier_percentage){
  set.seed(seed)
  corr_matrix <- matrix(0.1, nrow = p, ncol = p)
  diag(corr_matrix) <- 1
  #first 3 are highly correlated
  corr_matrix[1, 2] <- 0.9
  corr_matrix[1, 3] <- 0.9
  corr_matrix[2, 1] <- 0.9
  corr_matrix[3, 1] <- 0.9
  corr_matrix[2, 3] <- 0.9
  corr_matrix[3, 2] <- 0.9
  #4,5,6 are highly correlated
  corr_matrix[4, 5] <- 0.7
  corr_matrix[4, 6] <- 0.7
  corr_matrix[5, 4] <- 0.7
  corr_matrix[5, 6] <- 0.7
  corr_matrix[6, 4] <- 0.7
  corr_matrix[6, 5] <- 0.7
  #4 is not correlated with 1,2,3
  corr_matrix[1, 4] <- 0
  corr_matrix[2, 4] <- 0
  corr_matrix[3, 4] <- 0
  corr_matrix[4, 1] <- 0
  corr_matrix[4, 2] <- 0
  corr_matrix[4, 3] <- 0
  
  X <- mvrnorm(n, mu=rep(0,p),Sigma = corr_matrix)
  colnames(X)=paste0("X", 1:p, "")
  r.u <- runif(n)
  if(outlier==TRUE){
    num.out <- n*outlier_percentage#5%outliers
    gamma <- c(rep(0, n-num.out),  rep(5,ceiling(num.out)))
    ftime <- (-log(r.u)/((h0^k.shape)*exp(X %*% beta))+gamma)^(1/k.shape)
    ctime=runif(n,min=0,max=10)
    time <- pmin(ftime, ctime)
    status <- as.numeric(ftime <= ctime)
    current_data2=cbind.data.frame(time,status,X)
  } else{
    
    ftime <- (-log(r.u)/((h0^k.shape)*exp(X %*% beta)))^(1/k.shape)
    ctime=runif(n,min=0,max=10)
    time <- pmin(ftime, ctime)
    status <- as.numeric(ftime <= ctime)
    current_data2=cbind.data.frame(time,status,X)
  }
  return(current_data2)
}

#4

run_fun6_outlier=function(iter,model_name,outlier,h0,k.shape,outlier_percentage){
  current_data2=my_sim_data_0228(iter,n=300,p=1000,beta=c(0,0,3,0,-3,0,rep(0,1000-7),5),outlier,h0=0.5,k.shape=5,outlier_percentage=outlier_percentage)
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}

source("prcox_my.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=FALSE,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=FALSE,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=FALSE,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=FALSE,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=FALSE,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_param4_0813.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.001_param4_0813.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.002_param4_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.005_param4_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.01_param4_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.02_param4_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.05_param4_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.1_param4_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.2_param4_0813.RData")

#1
run_fun6_outlier=function(iter,model_name,outlier,h0,k.shape,outlier_percentage){
  current_data2=my_sim_data_0228(iter,n=300,p=1000,beta=c(10,-3,3,-10,-3,3,rep(0,1000-7),5),outlier,h0=0.5,k.shape=5,outlier_percentage=outlier_percentage)
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}

source("prcox_my.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=FALSE,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=FALSE,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=FALSE,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=FALSE,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=FALSE,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_param1_0813.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.001_param1_0813.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.002_param1_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.005_param1_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.01_param1_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.02_param1_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.05_param1_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.1_param1_0813.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.2_param1_0813.RData")

#2
run_fun6_outlier=function(iter,model_name,outlier,h0,k.shape,outlier_percentage){
  current_data2=my_sim_data_0228(iter,n=300,p=1000,beta=c(10,0,3,-10,-3,0,rep(0,1000-7),5),outlier,h0=0.5,k.shape=5,outlier_percentage=outlier_percentage)
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}

source("prcox_my.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=FALSE,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=FALSE,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=FALSE,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=FALSE,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=FALSE,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_param2_0819.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.001_param2_0819.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.002_param2_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.005_param2_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.01_param2_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.02_param2_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.05_param2_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.1_param2_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.2_param2_0819.RData")

#3

run_fun6_outlier=function(iter,model_name,outlier,h0,k.shape,outlier_percentage){
  current_data2=my_sim_data_0228(iter,n=300,p=1000,beta=c(2,-3,3,-2,-3,3,rep(0,1000-7),2),outlier,h0=0.5,k.shape=5,outlier_percentage=outlier_percentage)
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}

source("prcox_my.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=FALSE,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=FALSE,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=FALSE,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=FALSE,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=FALSE,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_param3_0819.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.001_param3_0819.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.002_param3_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.005_param3_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.01_param3_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.02_param3_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.05_param3_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.1_param3_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.2_param3_0819.RData")

#5
run_fun6_outlier=function(iter,model_name,outlier,h0,k.shape,outlier_percentage){
  current_data2=my_sim_data_0228(iter,n=300,p=1000,beta=c(10,0,0,10,0,0,rep(0,1000-7),5),outlier,h0=0.5,k.shape=5,outlier_percentage=outlier_percentage)
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}

source("prcox_my.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=FALSE,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=FALSE,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=FALSE,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=FALSE,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=FALSE,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_param5_0819.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.001_param5_0819.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.002_param5_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.005_param5_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.01_param5_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.02_param5_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.05_param5_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.1_param5_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.2_param5_0819.RData")

#6
run_fun6_outlier=function(iter,model_name,outlier,h0,k.shape,outlier_percentage){
  current_data2=my_sim_data_0228(iter,n=300,p=1000,beta=c(2,0,0,-2,0,0,rep(0,1000-7),2),outlier,h0=0.5,k.shape=5,outlier_percentage=outlier_percentage)
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}

source("prcox_my.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=FALSE,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=FALSE,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=FALSE,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=FALSE,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=FALSE,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_param6_0819.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.001,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.001_param6_0819.RData")


source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.002,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.002_param6_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.005,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.005_param6_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.01,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.01_param6_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.02,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.02_param6_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.05,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.05_param6_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.1,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.1_param6_0819.RData")

source("prcox.R")
result1=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="lasso", outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result2=pbmcapply::pbmclapply(1:500, run_fun6_outlier, model_name="enet",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result3=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="ncvreg",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result4=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="pawph",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)
result5=pbmcapply::pbmclapply(1:500, run_fun6_outlier,model_name="SIS",outlier=TRUE,outlier_percentage=0.2,mc.cores = 15)

save(result1,result2,result3,result4,result5,file="simdata_highdim_outlier0.2_param6_0819.RData")

# pawph

sim_fun=function(seed,n,p,beta,outlier){
  set.seed(seed)
  #beta <- c(1, -2, 1, rep(0, p-3))
  ## generate x matrix
  mu.x <- rep(0, p)
  Sigma.x <- matrix(rep(0.1,p*p),p,p)+diag(0.9,p)
  X <- mvrnorm(n, mu=mu.x, Sigma = Sigma.x)
  if(outlier==TRUE){
    ## generate failure time with long-survival outliers
    num.out <- n*0.05
    gamma <- c(rep(0, n-num.out),  rep(-5,ceiling(num.out)))
    k.shape <- 1
    h0 <- 2
    r.u <- runif(n)
    ftime <- (-log(r.u)/(h0^k.shape*exp(X %*% beta + gamma )))^(1/k.shape)#exp baseline hazard
    delta <- c(rep(0, n*0.2), rep(1,n*0.8 ))
    y <- Surv(ftime, delta) 
    colnames(X)=paste0("X", 1:p, "")
    time=ftime
    status=delta
    current_data2=cbind.data.frame(time,status,X)}else{
      
      k.shape <- 1
      h0 <- 2
      r.u <- runif(n)
      ftime <- (-log(r.u)/(h0^k.shape*exp(X %*% beta)))^(1/k.shape)
      delta <- c(rep(0, n*0.2), rep(1,n*0.8 ))
      y <- Surv(ftime, delta) 
      colnames(X)=paste0("X", 1:p, "")
      time=ftime
      status=delta
      current_data2=cbind.data.frame(time,status,X)
    }
  return(current_data2)
}

run_fun=function(iter, n,p,beta,outlier,model_name){
  current_data2=sim_fun(seed=iter,n=n,p=p,outlier =outlier,beta=beta)
  result_cal_result=result_cal(current_data2,model_name = model_name)
  return(result_cal_result)
}


result1=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="lasso",outlier=FALSE, mc.cores = 15)
result2=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="enet",outlier=FALSE, mc.cores = 15)
result3=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="alasso",outlier=FALSE, mc.cores = 15)
result4=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="aenet",outlier=FALSE, mc.cores = 15)
result5=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="mcp",outlier=FALSE, mc.cores = 15)
result6=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="scad",outlier=FALSE, mc.cores = 15)
result7=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="mnet",outlier=FALSE, mc.cores = 15)
result8=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="snet",outlier=FALSE, mc.cores = 15)
source("pawph.R")
result9=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="pawph", outlier=FALSE,mc.cores = 15)
source("pawph_mcp.R")
result10=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="pawph", outlier=FALSE,mc.cores = 15)
source("pawph_scad.R")
result11=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)), model_name="pawph",outlier=FALSE,mc.cores = 15)
result12=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="SIS",outlier=FALSE,mc.cores = 15)

save(result1,result2,result3,result4,result5,result6,result7,result8,result9,result10,result11,result12,file="pawphsimdata_lowdim_0320.RData")

result1=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="lasso",outlier=TRUE, mc.cores = 15)
result2=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="enet",outlier=TRUE, mc.cores = 15)
result3=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="alasso",outlier=TRUE, mc.cores = 15)
result4=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="aenet",outlier=TRUE, mc.cores = 15)
result5=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="mcp",outlier=TRUE, mc.cores = 15)
result6=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="scad",outlier=TRUE, mc.cores = 15)
result7=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="mnet",outlier=TRUE, mc.cores = 15)
result8=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="snet",outlier=TRUE, mc.cores = 15)
source("pawph.R")
result9=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="pawph", outlier=TRUE,mc.cores = 15)
source("pawph_mcp.R")
result10=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="pawph", outlier=TRUE,mc.cores = 15)
source("pawph_scad.R")
result11=pbmcapply::pbmclapply(1:100,run_fun, n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="pawph",outlier=TRUE,mc.cores = 15)
result12=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=30,beta=c(1,2,-1, rep(0, 30-3)),model_name="SIS",outlier=TRUE,mc.cores = 15)

save(result1,result2,result3,result4,result5,result6,result7,result8,result9,result10,result11,result12,file="pawphsimdata_lowdim_outlier_0320.RData")


result1=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="lasso",outlier=FALSE, mc.cores = 15)
result2=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="enet",outlier=FALSE, mc.cores = 15)
result3=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="alasso",outlier=FALSE, mc.cores = 15)
result4=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="aenet",outlier=FALSE, mc.cores = 15)
result5=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="mcp",outlier=FALSE, mc.cores = 15)
result6=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="scad",outlier=FALSE, mc.cores = 15)
result7=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="mnet",outlier=FALSE, mc.cores = 15)
result8=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="snet",outlier=FALSE, mc.cores = 15)
source("pawph.R")
result9=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="pawph", outlier=FALSE,mc.cores = 15)
source("pawph_mcp.R")
result10=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="pawph", outlier=FALSE,mc.cores = 15)
source("pawph_scad.R")
result11=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)), model_name="pawph",outlier=FALSE,mc.cores = 15)
result12=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="SIS",outlier=FALSE,mc.cores = 15)

save(result1,result2,result3,result4,result5,result6,result7,result8,result9,result10,result11,result12,file="pawphsimdata_highdim_0320.RData")


result1=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="lasso",outlier=TRUE, mc.cores = 15)
result2=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="enet",outlier=TRUE, mc.cores = 15)
result3=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="alasso",outlier=TRUE, mc.cores = 15)
result4=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="aenet",outlier=TRUE, mc.cores = 15)
result5=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="mcp",outlier=TRUE, mc.cores = 15)
result6=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="scad",outlier=TRUE, mc.cores = 15)
result7=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="mnet",outlier=TRUE, mc.cores = 15)
result8=pbmcapply::pbmclapply(1:100, run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="snet",outlier=TRUE, mc.cores = 15)
source("pawph.R")
result9=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="pawph", outlier=TRUE,mc.cores = 15)
source("pawph_mcp.R")
result10=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="pawph", outlier=TRUE,mc.cores = 15)
source("pawph_scad.R")
result11=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)), model_name="pawph",outlier=TRUE,mc.cores = 15)
result12=pbmcapply::pbmclapply(1:100,run_fun,n=300,p=1000,beta=c(1,2,-1, rep(0, 1000-3)),model_name="SIS",outlier=TRUE,mc.cores = 15)

save(result1,result2,result3,result4,result5,result6,result7,result8,result9,result10,result11,result12,file="pawphsimdata_highdim_outlier_0320.RData")

# SIS
sis_sim_data_case1=function(seed,n,p,beta){
  set.seed(seed)
  X <- mvrnorm(n, mu=rep(0,p),Sigma = diag(p))
  colnames(X)=paste0("X", 1:p, "")
  k.shape <- 1
  h0 <- 0.1
  r.u <- runif(n)
  ftime <- (-log(r.u)/(h0^k.shape*exp(X %*% beta)))^(1/k.shape)
  ctime=rpois(n,10)
  time <- pmin(ftime, ctime)
  status <- as.numeric(ftime <= ctime)
  current_data2=cbind.data.frame(time,status,X)
  return(current_data2)
}

sis_sim_data_case1_attempt=sis_sim_data_case1(1,n=300,p=400,beta=c(-1.6328,1.3988,-1.6497,1.6353,-1.4209,1.7022,rep(0,400-6)))
table(sis_sim_data_case1_attempt$status)#censoring rate 46%
summary(sis_sim_data_case1_attempt$time)
hist(sis_sim_data_case1_attempt$time)


sis_sim_data_case2=function(seed,n,p,beta){
  set.seed(seed)
  cov_matrix <- matrix(0.5, nrow = p, ncol = p)
  diag(cov_matrix) <- 1
  X <- mvrnorm(n, mu=rep(0,p),Sigma = cov_matrix)
  colnames(X)=paste0("X", 1:p, "")
  k.shape <- 1
  h0 <- 0.1
  r.u <- runif(n)
  ftime <- (-log(r.u)/(h0^k.shape*exp(X %*% beta)))^(1/k.shape)
  ctime=rpois(n,10)
  time <- pmin(ftime, ctime)
  status <- as.numeric(ftime <= ctime)
  current_data2=cbind.data.frame(time,status,X)
  return(current_data2)
}

sis_sim_data_case2_attempt=sis_sim_data_case2(1,n=300,p=400,beta=c(-1.6328,1.3988,-1.6497,1.6353,-1.4209,1.7022,rep(0,400-6)))
table(sis_sim_data_case2_attempt$status)#censoring rate 46%
summary(sis_sim_data_case2_attempt$time)
hist(sis_sim_data_case2_attempt$time)


sis_sim_data_case3=function(seed,n,p,beta){
  set.seed(seed)
  corr_matrix <- matrix(1/2, nrow = p, ncol = p)
  diag(corr_matrix) <- 1
  corr_matrix[, 4] <- 1/sqrt(2)
  corr_matrix[4, ] <- 1/sqrt(2)
  corr_matrix <- nearPD(corr_matrix)$mat
  
  X <- mvrnorm(n, mu=rep(0,p),Sigma = corr_matrix)
  colnames(X)=paste0("X", 1:p, "")
  k.shape <- 1
  h0 <- 0.1
  r.u <- runif(n)
  ftime <- (-log(r.u)/(h0^k.shape*exp(X %*% beta)))^(1/k.shape)
  ctime=rpois(n,10)
  time <- pmin(ftime, ctime)
  status <- as.numeric(ftime <= ctime)
  current_data2=cbind.data.frame(time,status,X)
  return(current_data2)
}

sis_sim_data_case3_attempt=sis_sim_data_case3(1,n=300,p=400,beta=c(4,4,4,-6*sqrt(2),rep(0,400-4)))
table(sis_sim_data_case3_attempt$status)#censoring rate 46%
summary(sis_sim_data_case3_attempt$time)
hist(sis_sim_data_case3_attempt$time)


sis_sim_data_case4=function(seed,n,p,beta){
  set.seed(seed)
  corr_matrix <- matrix(1/2, nrow = p, ncol = p)
  diag(corr_matrix) <- 1
  corr_matrix[, 4] <- 1/sqrt(2)
  corr_matrix[4, ] <- 1/sqrt(2)
  corr_matrix[, 5] <- 0
  corr_matrix[5, ] <- 0
  corr_matrix <- nearPD(corr_matrix)$mat
  
  X <- mvrnorm(n, mu=rep(0,p),Sigma = corr_matrix)
  colnames(X)=paste0("X", 1:p, "")
  k.shape <- 1
  h0 <- 0.1
  r.u <- runif(n)
  ftime <- (-log(r.u)/(h0^k.shape*exp(X %*% beta)))^(1/k.shape)
  ctime=rpois(n,10)
  time <- pmin(ftime, ctime)
  status <- as.numeric(ftime <= ctime)
  current_data2=cbind.data.frame(time,status,X)
  return(current_data2)
}

run_fun3=function(iter,model_name){
  current_data2=sis_sim_data_case1(iter,n=300,p=400,beta=c(-1.6328,1.3988,-1.6497,1.6353,-1.4209,1.7022,rep(0,400-6)))
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}
run_fun4=function(iter,model_name){
  current_data2=sis_sim_data_case2(iter,n=300,p=400,beta=c(-1.6328,1.3988,-1.6497,1.6353,-1.4209,1.7022,rep(0,400-6)))
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}
run_fun5=function(iter,model_name){
  current_data2=sis_sim_data_case3(iter,n=300,p=400,beta=c(4,4,4,-6*sqrt(2),rep(0,400-4)))
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}
run_fun6=function(iter,model_name){
  current_data2=sis_sim_data_case4(iter,n=300,p=400,beta=c(4,4,4,-6*sqrt(2),4/3,rep(0,400-5)))
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}
run_fun7=function(iter,model_name){
  current_data2=sis_sim_data_case2(iter,n=400,p=1000,beta=c(-1.5140,1.2799,-1.5307,1.5164,-1.3020,1.5833,rep(0,1000-6)))
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}
run_fun8=function(iter,model_name){
  current_data2=sis_sim_data_case4(iter,n=400,p=1000,beta=c(4,4,4,-6*sqrt(2),4/3,rep(0,1000-5)))
  result_cal_result=result_cal(current_data2,model_name)
  return(result_cal_result)
}

result1=pbmcapply::pbmclapply(1:100, run_fun3,model_name="lasso",mc.cores = 15)
result2=pbmcapply::pbmclapply(1:100, run_fun3,model_name="enet",mc.cores = 15)
result3=pbmcapply::pbmclapply(1:100, run_fun3,model_name="alasso", mc.cores = 15)
result4=pbmcapply::pbmclapply(1:100, run_fun3,model_name="aenet",mc.cores = 15)
result5=pbmcapply::pbmclapply(1:100, run_fun3,model_name="mcp", mc.cores = 15)
result6=pbmcapply::pbmclapply(1:100, run_fun3,model_name="scad",mc.cores = 15)
result7=pbmcapply::pbmclapply(1:100, run_fun3,model_name="mnet", mc.cores = 15)
result8=pbmcapply::pbmclapply(1:100, run_fun3,model_name="snet",mc.cores = 15)
source("pawph.R")
result9=pbmcapply::pbmclapply(1:100, run_fun3,model_name="pawph",mc.cores = 15)
source("pawph_mcp.R")
result10=pbmcapply::pbmclapply(1:100, run_fun3,model_name="pawph_mcp",mc.cores = 15)
source("pawph_scad.R")
result11=pbmcapply::pbmclapply(1:100, run_fun3,model_name="pawph_scad",mc.cores = 15)
result12=pbmcapply::pbmclapply(1:100, run_fun3,model_name="SIS",mc.cores = 15)

save(result1,result2,result3,result4,result5,result6,result7,result8,result9,result10,result11,result12,file="sissimdata1_0813.RData")


result1=pbmcapply::pbmclapply(1:100, run_fun5,model_name="lasso",mc.cores = 15)
result2=pbmcapply::pbmclapply(1:100, run_fun5,model_name="enet",mc.cores = 15)
result3=pbmcapply::pbmclapply(1:100, run_fun5,model_name="alasso", mc.cores = 15)
result4=pbmcapply::pbmclapply(1:100, run_fun5,model_name="aenet",mc.cores = 15)
result5=pbmcapply::pbmclapply(1:100, run_fun5,model_name="mcp", mc.cores = 15)
result6=pbmcapply::pbmclapply(1:100, run_fun5,model_name="scad",mc.cores = 15)
result7=pbmcapply::pbmclapply(1:100, run_fun5,model_name="mnet", mc.cores = 15)
result8=pbmcapply::pbmclapply(1:100, run_fun5,model_name="snet",mc.cores = 15)
result9=pbmcapply::pbmclapply(1:100, run_fun5,model_name="pawph",mc.cores = 15)
result10=pbmcapply::pbmclapply(1:100, run_fun5,model_name="pawph_mcp",mc.cores = 15)
result11=pbmcapply::pbmclapply(1:100, run_fun5,model_name="pawph_scad",mc.cores = 15)
result12=pbmcapply::pbmclapply(1:100, run_fun5,model_name="SIS",mc.cores = 15)

save(result1,result2,result3,result4,result5,result6,result7,result8,result9,result10,result11,result12,file="sissimdata3_813.RData")



