rescale_minMax <- function(x){(x-min(x))/(max(x)-min(x))}

library(glmnet)
library(caret)
ElasticNet.model <- function(x, y) {
  
  rangey <- diff(range(y))
  ys <- rescale_minMax(y) * 2 - 1
  y_var <- data.frame(y=ys)
  
  xs <- matrix(ncol = ncol(x), nrow = nrow(x))
  for (i in seq_len(ncol(x))) {
    xs[, i] <- rescale_minMax(x[,i])
  }
  x_vars <- xs
  
  x.train.sub <- data.matrix(x_vars)
  colnames(x.train.sub) <- colnames(x)
  y.train.sub <- data.matrix(y_var)
  
  srchGrid <- expand.grid(alpha = 0.5, lambda = 10^seq(2, -3, by = -.1))
  
  model <- train(
    x=x.train.sub, y = y.train.sub[,1], method = "glmnet", 
    standardize = FALSE, standardize.response = FALSE,
    trControl = trainControl("cv", number = nrow(x.train.sub)),
    tuneGrid = srchGrid
  )
  
  model.coef <- as.matrix(coef(model$finalModel, s=model$bestTune$lambda))
  destandardized_coef <- rangey/apply(x, 2, function(x) diff(range(x)))*model.coef[-1]
  # personal_standardize <- c(mean(y) - sum(destandardized_coef * colMeans(x)),
  #                           destandardized_coef)
  
  coefs <- data.frame(Predictor = names(destandardized_coef), Value= destandardized_coef)
  
  return(list(pars = model$bestTune, coef=coefs))
}



