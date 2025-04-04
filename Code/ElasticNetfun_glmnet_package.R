
library(glmnet)

# elastic net 
ElasticNet.model <- function(x_vars, y, 
                             lambda_grid = 10^seq(2, -3, by = -.1),
                             alpha_grid = seq(1, 0, length.out=20),
                             standardize = TRUE, nfold=10,
                             seed=1111) {
  
  set.seed(seed)
  foldid <- sample(1:nfold, size = length(y), replace = TRUE)
  
  res_df <- data.frame()
  model_ls <- list()
  
  for (thisalpha in alpha_grid) {
    cv1=cv.glmnet(x=x_vars, y=y, foldid=foldid, 
                  alpha=thisalpha, lambda = lambda_grid, 
                  type.measure = "mse", standardize = standardize)
    model_ls[[which(alpha_grid==thisalpha)]] <- cv1
    df <- data.frame(alpha=thisalpha, lambda=cv1$lambda, mse=cv1$cvm, nzero=cv1$nzero)
    rownames(df) <- NULL
    res_df <- as.data.frame(rbind(res_df, df))
  }
  besttune <- res_df[which(res_df$mse==min(res_df$mse)),]
  onebesttune <- besttune[which.min(besttune$nzero),]
  
  bestfit <- model_ls[[which(alpha_grid==onebesttune$alpha)]]
  model.coef <- as.matrix(coef(bestfit, s=onebesttune$lambda))
  coefs <- data.frame(Predictor = names(model.coef[,1]), Value= model.coef[,1])[-1,]
  
  return(list(cvres=res_df, pars = onebesttune, 
              coef=coefs, intercept=model.coef[1], 
              cvmodels=model_ls, finalmodel=bestfit))
}
  

# elastic net with weights
ElasticNet_weight.model <- function(x_vars, y, 
                             lambda_grid = 10^seq(2, -3, by = -.1),
                             alpha_grid = seq(1, 0, length.out=20),
                             standardize = TRUE, nfold=10,
                             penalty.factor,
                             seed=1111) {
  
  set.seed(seed)
  foldid <- sample(1:nfold, size = length(y), replace = TRUE)
  
  res_df <- data.frame()
  model_ls <- list()
  
  for (thisalpha in alpha_grid) {
    cv1=cv.glmnet(x=x_vars, y=y, foldid=foldid, penalty.factor=penalty.factor,
                  alpha=thisalpha, lambda = lambda_grid, 
                  type.measure = "mse", standardize = standardize)
    model_ls[[which(alpha_grid==thisalpha)]] <- cv1
    df <- data.frame(alpha=thisalpha, lambda=cv1$lambda, mse=cv1$cvm, nzero=cv1$nzero)
    rownames(df) <- NULL
    res_df <- as.data.frame(rbind(res_df, df))
  }
  besttune <- res_df[which(res_df$mse==min(res_df$mse)),]
  onebesttune <- besttune[which.min(besttune$nzero),]
  
  bestfit <- model_ls[[which(alpha_grid==onebesttune$alpha)]]
  model.coef <- as.matrix(coef(bestfit, s=onebesttune$lambda))
  coefs <- data.frame(Predictor = names(model.coef[,1]), Value= model.coef[,1])[-1,]
  
  return(list(cvres=res_df, pars = onebesttune, 
              coef=coefs, intercept=model.coef[1], 
              cvmodels=model_ls, finalmodel=bestfit))
}

# accuracy evaluation
eval_results <- function(true, predicted) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  
  return(R_square)
}

# lasso
Lasso.model <- function(x_vars, y, 
                        lambda_grid = 10^seq(2, -3, by = -.1),
                        standardize = TRUE, nfold=10,
                        seed=1111) {
  ElasticNet.model(x_vars, y, 
                   lambda_grid = lambda_grid,
                   alpha_grid = 1,
                   standardize = standardize, nfold=nfold,
                   seed=seed)
}


# random lasso
library(glmnet)
RL_step1 <- function(x_vars, y_var, times=200, 
                     lambda_grid = 10^seq(2, -3, by = -.1),
                     nfold=10, seed=1111) {
  
  set.seed(seed)
  y_var <- data.frame(y=y_var)
  test_splits <- list()
  
  q1 <- ceiling(runif(1,0.15,0.20)*ncol(x_vars))
  q1 <- max(2, q1)
  
  vec_r2 <- c()
  for(k in 1:times) {
    train_rows2 <- sample(1:nrow(x_vars), nrow(x_vars), replace = TRUE)
    x.train.sub <- data.matrix(x_vars[train_rows2, ])
    y.train.sub <- data.matrix(y_var[train_rows2,])
    
    step1_cols <- sample(1:ncol(x_vars), q1)
    x.train.sub <- x.train.sub[,step1_cols]
    
    lasso_reg <- Lasso.model(x.train.sub, y.train.sub, lambda_grid = lambda_grid, nfold = nfold)
    
    y_predicted <- predict(lasso_reg$finalmodel, s = lasso_reg$pars$lambda, newx = x.train.sub)
    r2 <- eval_results(true=y.train.sub, predicted=y_predicted)
    
    vec_r2 <- c(vec_r2, r2)
    test_splits[[k]] <- model.coef
  }
  
  res_mat <- matrix(NA, nrow = times, ncol = ncol(x_vars))
  colnames(res_mat) <- colnames(x_vars)
  
  for (i in 1:times) {
    res1 <- test_splits[[i]]
    res_mat[i, res1$Predictor] <- res1$Value
  }
  
  return(list(res=res_mat, r2=vec_r2))
}


RL_step2 <- function(x_vars, y_var, imp_weight, times=200, 
                     lambda_grid = 10^seq(2, -3, by = -.1),
                     nfold=10, seed=1111) {
  
  set.seed(seed)
  
  q2 <- ceiling(runif(1,0.15,0.20)*ncol(x_vars))
  q2 <- max(2, q2)
  y_var <- data.frame(y=y_var)
  
  test_splits <- list()
  vec_r2 <- c()
  for(k in 1:times) {
    train_rows2 <- sample(1:nrow(x_vars), nrow(x_vars), replace = TRUE)
    x.train.sub <- data.matrix(x_vars[train_rows2, ])
    y.train.sub <- data.matrix(y_var[train_rows2,])
    
    step2_cols <- sample(1:ncol(x_vars), q2, prob=imp_weight)
    
    x.train.sub <- x.train.sub[,step2_cols]
    
    lasso_reg <- Lasso.model(x.train.sub, y.train.sub, lambda_grid = lambda_grid, nfold = nfold)
    model.coef <- lasso_reg$coef
    
    y_predicted <- predict(lasso_reg$finalmodel, s = lasso_reg$pars$lambda, newx = x.train.sub)
    r2 <- eval_results(true=y.train.sub, predicted=y_predicted)
    
    vec_r2 <- c(vec_r2, r2)
    test_splits[[k]] <- model.coef
  }
  
  res_mat <- matrix(NA, nrow = times, ncol = ncol(x_vars))
  colnames(res_mat) <- colnames(x_vars)
  
  for (i in 1:times) {
    res1 <- test_splits[[i]]
    res_mat[i, res1$Predictor] <- res1$Value
  }
  
  return(list(res=res_mat, r2=vec_r2))
}

