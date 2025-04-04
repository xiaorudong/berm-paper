# -------------------------------
# get task ID from slurm
# --------------------------------
t <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))


errorsd_choice <- c(1, 3, 5)
s_vec <- c("s1", "s2", "s3", "s4", "s5")


# lambda_grid <- 10^seq(2, -3, by = -.1)
# alpha_grid <- seq(1, 0, length.out=20)
# srchGrid <- expand.grid(alpha = alpha_grid, lambda = lambda_grid)

library(caret)
library(glmnet)
library(pense)


# setwd("/Volumes/bachergroup/HDDataSelection_Method/")
source("CODE/fixed_caretglmnet/ElasticNetfun_caret_package.R")

for (sd in errorsd_choice) {
  for (s in s_vec) {
    
    filepath <- paste("RDATA/runif_hip+/60vars300obs_sd", sd, "/sim_hip_60vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    load(file = filepath)
    
    t1 <- Sys.time()
    res_elnet <- ElasticNet.model(X_mat, y)
    coef_res_elnet <- res_elnet$coef
    coef_res_elnet$penalty <- abs(1/coef_res_elnet$Value)
    coef_res_elnet <- coef_res_elnet[colnames(X_mat),]
    
    if(sum(coef_res_elnet$penalty!=Inf)>0) {
      specialalpha <- c(seq(1, 0, length.out=20)[-20], 0.000001)
      cv_results <- elnet_cv(X_mat, y, alpha = specialalpha,
                             lambda = 10^seq(2, -3, by = -.1), cv_k = 10, 
                             penalty_loadings=coef_res_elnet$penalty)
      cv_coef <- coef(cv_results)[-1]
    }
    if(sum(coef_res_elnet$penalty!=Inf)==0) {
      cv_coef <- rep(0, ncol(X_mat))
      names(cv_coef) <- colnames(X_mat)
    }
    
    t2 <- Sys.time()
    difft <- difftime(t2, t1, units = "mins")
    
    
    # summary
    Adaptive_EN <- data.frame(VAR = names(cv_coef), est = cv_coef)
    truth <- data.frame(truecoef=beta, VAR=paste("X", 1:length(beta), sep = ""))
    Adaptive_EN <- merge(truth, Adaptive_EN, by="VAR")
    Adaptive_EN$FROM <- rep("Adaptive_EN", nrow(Adaptive_EN))
    Adaptive_EN <- Adaptive_EN[,c("truecoef", "est", "FROM", "VAR")]
    
    filepath <- paste("RDATA/fixed_caret/onlyres_adaptiveEN_hip_60vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    save(Adaptive_EN, difft, file=filepath)
  }
}







#####

for (sd in errorsd_choice) {
  for (s in s_vec) {
    
    filepath <- paste("RDATA/runif_hip+/500vars300obs_sd", sd, "/sim_hip_500vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    load(file = filepath)
    
    t1 <- Sys.time()
    res_elnet <- ElasticNet.model(X_mat, y)
    coef_res_elnet <- res_elnet$coef
    coef_res_elnet$penalty <- abs(1/coef_res_elnet$Value)
    coef_res_elnet <- coef_res_elnet[colnames(X_mat),]
    
    if(sum(coef_res_elnet$penalty!=Inf)>0) {
      specialalpha <- c(seq(1, 0, length.out=20)[-20], 0.000001)
      cv_results <- elnet_cv(X_mat, y, alpha = specialalpha,
                             lambda = 10^seq(2, -3, by = -.1), cv_k = 10, 
                             penalty_loadings=coef_res_elnet$penalty)
      cv_coef <- coef(cv_results)[-1]
    }
    if(sum(coef_res_elnet$penalty!=Inf)==0) {
      cv_coef <- rep(0, ncol(X_mat))
      names(cv_coef) <- colnames(X_mat)
    }
    
    t2 <- Sys.time()
    difft <- difftime(t2, t1, units = "mins")
    
    
    # summary
    Adaptive_EN <- data.frame(VAR = names(cv_coef), est = cv_coef)
    truth <- data.frame(truecoef=beta, VAR=paste("X", 1:length(beta), sep = ""))
    Adaptive_EN <- merge(truth, Adaptive_EN, by="VAR")
    Adaptive_EN$FROM <- rep("Adaptive_EN", nrow(Adaptive_EN))
    Adaptive_EN <- Adaptive_EN[,c("truecoef", "est", "FROM", "VAR")]
    
    filepath <- paste("RDATA/fixed_caret/onlyres_adaptiveEN_hip_500vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    save(Adaptive_EN, difft, file=filepath)
  }
}