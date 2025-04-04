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
library(resamplr)
library(tidyverse)
library(coxed)


# setwd("/Volumes/bachergroup/HDDataSelection_Method/")
source("CODE/fixed_caretglmnet/ENfun_forRENBOOT_caret.R")

for (sd in errorsd_choice) {
  for (s in s_vec) {
    
    filepath <- paste("RDATA/runif_hip+/60vars300obs_sd", sd, "/sim_hip_60vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    load(file = filepath)
    
    
    t1 <- Sys.time()
    
    K=100
    boot <- balanced_bootstrap(as.data.frame(cbind(y, X_mat)), R=K)
    
    # bootstrapping
    res_ls <- list()
    pars_ls <- list()
    for (i in 1:K) {
      #Creating a resampled dataset from the sample data
      train_rows <- boot[[1]][[i]][["idx"]]
      x.train.sub <- X_mat[train_rows, ]
      y.train.sub <- y[train_rows,]
      
      #Running the regression on these data
      model_bootstrap <- ElasticNet.model(x.train.sub, y.train.sub)
      res_ls[[i]] <- model_bootstrap$coef
      pars_ls[[i]] <- model_bootstrap$pars
    }
    
    res_df <- res_ls %>% reduce(full_join, by = "Predictor") %>% column_to_rownames(var = "Predictor") %>% as.data.frame()
    colnames(res_df) <- 1:K
    ci_df <- data.frame(t(apply(res_df, 1, bca)))
    colnames(ci_df) <- c("lw", "up")
    ci_df[apply(res_df, 1, function(x) length(unique(x))==1), "lw"] <- res_df[apply(res_df, 1, function(x) length(unique(x))==1), 1]
    ci_df[apply(res_df, 1, function(x) length(unique(x))==1), "up"] <- res_df[apply(res_df, 1, function(x) length(unique(x))==1), 1]
    
    ci_df$relev <- ifelse(ci_df$lw<=0 & ci_df$up>=0, F, T)
    ci_df$est <- apply(res_df, 1, function(x) abs(mean(x)))
    ci_df$est <- ifelse(ci_df$relev, ci_df$est, 0)
    
    t2 <- Sys.time()
    difft <- difftime(t2, t1, units = "mins")
    
    truth <- data.frame(truecoef=beta, VAR=paste("X", 1:length(beta), sep = ""))
    Renboot <- ci_df %>% rownames_to_column()  %>% merge(x=truth, by.y = "rowname", by.x = "VAR")
    Renboot$FROM <- rep("Renboot", nrow(Renboot))
    Renboot <- Renboot[,c("truecoef", "est", "FROM", "VAR")]
    
    filepath <- paste("RDATA/fixed_caret/onlyres_RENBOOT_hip_60vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    save(Renboot, difft, file=filepath)
  }
}






#####

for (sd in errorsd_choice) {
  for (s in s_vec) {
    
    filepath <- paste("RDATA/runif_hip+/500vars300obs_sd", sd, "/sim_hip_500vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    load(file = filepath)
    
    
    t1 <- Sys.time()
    
    K=100
    boot <- balanced_bootstrap(as.data.frame(cbind(y, X_mat)), R=K)
    
    # bootstrapping
    res_ls <- list()
    pars_ls <- list()
    for (i in 1:K) {
      #Creating a resampled dataset from the sample data
      train_rows <- boot[[1]][[i]][["idx"]]
      x.train.sub <- X_mat[train_rows, ]
      y.train.sub <- y[train_rows,]
      
      #Running the regression on these data
      model_bootstrap <- ElasticNet.model(x.train.sub, y.train.sub)
      res_ls[[i]] <- model_bootstrap$coef
      pars_ls[[i]] <- model_bootstrap$pars
    }
    
    res_df <- res_ls %>% reduce(full_join, by = "Predictor") %>% column_to_rownames(var = "Predictor") %>% as.data.frame()
    colnames(res_df) <- 1:K
    ci_df <- data.frame(t(apply(res_df, 1, bca)))
    colnames(ci_df) <- c("lw", "up")
    ci_df[apply(res_df, 1, function(x) length(unique(x))==1), "lw"] <- res_df[apply(res_df, 1, function(x) length(unique(x))==1), 1]
    ci_df[apply(res_df, 1, function(x) length(unique(x))==1), "up"] <- res_df[apply(res_df, 1, function(x) length(unique(x))==1), 1]
    
    ci_df$relev <- ifelse(ci_df$lw<=0 & ci_df$up>=0, F, T)
    ci_df$est <- apply(res_df, 1, function(x) abs(mean(x)))
    ci_df$est <- ifelse(ci_df$relev, ci_df$est, 0)
    
    t2 <- Sys.time()
    difft <- difftime(t2, t1, units = "mins")
    
    truth <- data.frame(truecoef=beta, VAR=paste("X", 1:length(beta), sep = ""))
    Renboot <- ci_df %>% rownames_to_column()  %>% merge(x=truth, by.y = "rowname", by.x = "VAR")
    Renboot$FROM <- rep("Renboot", nrow(Renboot))
    Renboot <- Renboot[,c("truecoef", "est", "FROM", "VAR")]
    
    filepath <- paste("RDATA/fixed_caret/onlyres_RENBOOT_hip_500vars300obs_", t, "_sd", sd, "_", s, ".RData", sep = "")
    save(Renboot, difft, file=filepath)
  }
}
