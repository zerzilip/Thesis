logistic_fit <- function(data, ...) glm(y ~.-p, family = "binomial", data = data)
logistic_predict <- function(model, data) predict(model, data, type="response")

logistic_model <- list(fit = logistic_fit, predict = logistic_predict)


lda_fit <- function(data, ...) MASS::lda(y ~.-p, data = data)
lda_predict <- function(model, data) predict(model, data)$posterior[,2]

lda_model <- list(fit = lda_fit, predict = lda_predict)

ridge_fit <- function(data, ...) 
  glmnet::cv.glmnet(model.matrix(y ~ .-p, data), data$y, family = "binomial", alpha = 0)
ridge_predict <- function(model, data) 
  predict(model, model.matrix(y ~ .-p, data), type="response", s = "lambda.1se")[,1]
ridge_model <- list(fit = ridge_fit, predict = ridge_predict)

lasso_fit <- function(data, ...) 
  glmnet::cv.glmnet(model.matrix(y ~ .-p, data), data$y, family = "binomial", alpha = 1)
lasso_predict <- function(model, data) 
  predict(model, model.matrix(y ~ .-p, data), type="response", s = "lambda.1se")[,1]
lasso_model <- list(fit = lasso_fit, predict = lasso_predict)

make_gam_formula <- function(d, df_ = 4){
  n_pred <- ncol(d) - 2 #p and y are not predictors
  var_names <- colnames(d)[1:n_pred]
  if (length(df_) == n_pred) df_ <- rep(df_[1], n_pred)
  #df_ <- as.integer(df_)
  formula_ <- paste("s(",var_names,", df = ", df_, ")", sep = "")
  formula_ <- paste(formula_, collapse = " + ")
  formula_ <- paste("y ~", formula_)
  return(as.formula(formula_))
}

gam_fit <- function(data, df = 4)
  gam::gam(make_gam_formula(data, df), "binomial", data)
gam_predict <- function(model, data) predict(model, data, type="response")

make_gam_fit <- function(df_) {
    return(function(data) gam::gam(mgf(data, df_), "binomial", data))
}
   
  
gam_fit_df4 <- function(data)
  gam(as.formula(paste0("y~",paste0("s(V",1:(ncol(data)-2),
                             ", df=4)",collapse="+"))), "binomial", data)

gam_fit_df7 <- function(data)
  gam(as.formula(paste0("y~",paste0("s(V",1:(ncol(data)-2),
                             ", df=7)",collapse="+"))), "binomial", data)

gam_fit_df15 <- function(data)
  gam(as.formula(paste0("y~",paste0("s(V",1:(ncol(data)-2),
                             ", df=15)",collapse="+"))), "binomial", data)

gam_df4 <- list(fit = gam_fit_df4, predict = gam_predict) 
gam_df7 <- list(fit = gam_fit_df7, predict = gam_predict)
gam_df15 <- list(fit = gam_fit_df15, predict = gam_predict)
#gam_model <- list(fit = gam_fit, predict = gam_predict)

#as.formula(paste0("y~",paste0("s(x",1:p,")",collapse="+")))
