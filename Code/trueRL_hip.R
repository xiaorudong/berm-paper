# -------------------------------
# get task ID from slurm
# --------------------------------
t <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))


errorsd_choice <- c(1, 3, 5)
s_vec <- c("s1", "s2", "s3", "s4", "s5")

#lambda_grid <- 10^seq(2, -3, by = -.1)
#alpha_grid <- seq(1, 0, length.out=20)
#srchGrid <- expand.grid(alpha = alpha_grid, lambda = lambda_grid)

library(caret)
library(glmnet)


# setwd("/Volumes/bachergroup/HDDataSelection_Method/")
source("CODE/fixed_caretglmnet/ElasticNetfun_caret_package.R")

for (sd in errorsd_choice) {
  for (s in s_vec) {
    
    filepath <- paste("RDATA/runif_hip+/60vars300obs_sd", sd, "/sim_hip_60vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    load(file = filepath)

    t1 <- Sys.time()
    resRL_step1 <- RL_step1(X_mat, y, times=500)
    res_mat <- resRL_step1$res
    
    # step1: orig mean
    var_importance <- colSums(res_mat, na.rm = T)/nrow(res_mat)
    imp_weight_orig <- abs(var_importance)/sum(abs(var_importance))
    
    # step2
    resRL_step2 <- RL_step2(X_mat, y, imp_weight_orig, times=500)
    res_mat2_orig <- resRL_step2$res
    var_importance2_orig <- colSums(res_mat2_orig, na.rm = T)/nrow(res_mat2_orig)
    step2_orig_RL_RL <- data.frame(step2_weight=var_importance2_orig, FROM=rep("RL", length(var_importance2_orig)), VAR=names(var_importance2_orig))
    
    # cutoff explore:
    cutoff <- 1/nrow(X_mat)
    step2_orig_RL_RL$est <- ifelse(abs(step2_orig_RL_RL$step2_weight)>cutoff, step2_orig_RL_RL$step2_weight, 0)
    
    t2 <- Sys.time()
    difft <- difftime(t2, t1, units = "mins")
    
    RL <- step2_orig_RL_RL[,2:4]
    truth <- data.frame(truecoef=beta, VAR=paste("X", 1:length(beta), sep = ""))
    RL <- merge(truth, RL, by="VAR")
    RL <- RL[,c("truecoef", "est", "FROM", "VAR")]
    
    filepath <- paste("RDATA/fixed_caret/onlyres_trueRL_hip_60vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    save(RL, difft, file=filepath)
  }
}






#####

for (sd in errorsd_choice) {
  for (s in s_vec) {
    
    filepath <- paste("RDATA/runif_hip+/500vars300obs_sd", sd, "/sim_hip_500vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    load(file = filepath)
    
    t1 <- Sys.time()
    resRL_step1 <- RL_step1(X_mat, y, times=500)
    res_mat <- resRL_step1$res
    
    # step1: orig mean
    var_importance <- colSums(res_mat, na.rm = T)/nrow(res_mat)
    imp_weight_orig <- abs(var_importance)/sum(abs(var_importance))
    
    # step2
    resRL_step2 <- RL_step2(X_mat, y, imp_weight_orig, times=500)
    res_mat2_orig <- resRL_step2$res
    var_importance2_orig <- colSums(res_mat2_orig, na.rm = T)/nrow(res_mat2_orig)
    step2_orig_RL_RL <- data.frame(step2_weight=var_importance2_orig, FROM=rep("RL", length(var_importance2_orig)), VAR=names(var_importance2_orig))
    
    # cutoff explore:
    cutoff <- 1/nrow(X_mat)
    step2_orig_RL_RL$est <- ifelse(abs(step2_orig_RL_RL$step2_weight)>cutoff, step2_orig_RL_RL$step2_weight, 0)
    
    t2 <- Sys.time()
    difft <- difftime(t2, t1, units = "mins")
    
    RL <- step2_orig_RL_RL[,2:4]
    truth <- data.frame(truecoef=beta, VAR=paste("X", 1:length(beta), sep = ""))
    RL <- merge(truth, RL, by="VAR")
    RL <- RL[,c("truecoef", "est", "FROM", "VAR")]
    
    filepath <- paste("RDATA/fixed_caret/onlyres_trueRL_hip_500vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    save(RL, difft, file=filepath)
  }
}