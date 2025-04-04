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
    res_elasticNet <- ElasticNet.model(X_mat, y)
    t2 <- Sys.time()
    difft <- difftime(t2, t1, units = "mins")
    
    elasticNet <- data.frame(est=res_elasticNet$coef$Value, VAR=res_elasticNet$coef$Predictor)
    truth <- data.frame(truecoef=beta, VAR=paste("X", 1:length(beta), sep = ""))
    elasticNet <- merge(truth, elasticNet, by="VAR")
    elasticNet$FROM <- rep("elasticNet", nrow(elasticNet))
    elasticNet <- elasticNet[,c("truecoef", "est", "FROM", "VAR")]
    
    filepath <- paste("RDATA/fixed_caret/onlyres_elasticNet_hip_60vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    save(elasticNet, difft, file=filepath)
  }
}







#####

for (sd in errorsd_choice) {
  for (s in s_vec) {
    
    filepath <- paste("RDATA/runif_hip+/500vars300obs_sd", sd, "/sim_hip_500vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    load(file = filepath)
    
    t1 <- Sys.time()
    res_elasticNet <- ElasticNet.model(X_mat, y)
    t2 <- Sys.time()
    difft <- difftime(t2, t1, units = "mins")
    
    elasticNet <- data.frame(est=res_elasticNet$coef$Value, VAR=res_elasticNet$coef$Predictor)
    truth <- data.frame(truecoef=beta, VAR=paste("X", 1:length(beta), sep = ""))
    elasticNet <- merge(truth, elasticNet, by="VAR")
    elasticNet$FROM <- rep("elasticNet", nrow(elasticNet))
    elasticNet <- elasticNet[,c("truecoef", "est", "FROM", "VAR")]
    
    filepath <- paste("RDATA/fixed_caret/onlyres_elasticNet_hip_500vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    save(elasticNet, difft, file=filepath)
  }
}
