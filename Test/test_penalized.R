library(glmnet)
library(MASS)


# Required Functions ------------------------------------------------------



AUC <- function(y, prob) as.numeric(pROC::auc(y, prob, direction="<", quiet = TRUE))
Accuracy <- function(y, prob, t = .5) mean(y == (prob > t))

generate_data <- function(n, beta, cor_mat = NULL){
  n_pred <- length(beta) - 1 #numper of predictors
  x <- if (is.null(cor_mat)) matrix(rnorm(n*n_pred), n, n_pred) else
    mvrnorm(n, rep(0, n_pred), cor_mat)
  design_matrix <- cbind(1, x)
  p <- as.numeric(1/(1+exp(- design_matrix %*% beta))) #true probabilities from logit model
  y <- rbinom(n, 1, p) #drawing outcomes from Bernoulli distribution
  dat <- as.data.frame(x)
  dat$y <- y; dat$p <- p
  return(dat)
} 


#diagonal covariance matrix
create_covariance_matrix <- function(n_pred, n_cor){
  a <- c(rep(0, n_pred - n_cor), 1:n_cor, (n_cor-1):1, rep(0, n_pred - n_cor)) / n_cor
  sapply(n_pred:1, function(x) a[x:(x+n_pred-1)])
}
create_covariance_matrix(6, 4)

#auto regressive covariance matrix
create_exponential_correl_mat <- function(n_pred, gamma){
  a <- c((n_pred-1):0, 1:(n_pred-1))
  exp_mat <- sapply(n_pred:1, function(x) a[x:(x+n_pred-1)])
  gamma^exp_mat
}
create_exponential_correl_mat(6, .9)


# Testing functions and setup ---------------------------------------------


n_pred <- 10 #number of predictors
beta <- rep(0, n_pred+1)
beta[2:5] <- c(2, -2, 2, -2)
beta
correl_mat <- create_covariance_matrix(n_pred, 4)
correl_mat

d <- generate_data(10^5, beta, correl_mat)
#density of probabilties
plot(density(d$p)) #there are two nice bumps
#maximum possible accuracy
Accuracy(d$y, d$p)
#maximum possible AUC
AUC(d$y, d$p)

set.seed(100)
dat <- generate_data(100, beta, correl_mat)

logit_model <- glm(y ~. -p, family = "binomial", data = dat)
x <- model.matrix(y ~ .-p, dat)
#optimizing for binomial deviance
cv_ridge <- cv.glmnet(x, dat$y, family = "binomial", alpha = 0)
plot(cv_ridge)
#optimizing for accuracy
cv_ridge_acc <- cv.glmnet(x, dat$y, family = "binomial", alpha = 0, type.measure = "class")
plot(cv_ridge_acc)

#apparemt errors
p_sl <- predict(logit_model, dat, type="response")
p_r <- predict(cv_ridge, x, s = "lambda.1se", type="response")
p_r_a <- predict(cv_ridge_acc, x, s = "lambda.1se", type="response")

#apparent performance measures
Accuracy(dat$y, p_sl)
Accuracy(dat$y, p_r) #Apparent accuracy is lower as expected
Accuracy(dat$y, p_r_a)

AUC(dat$y, p_sl)
AUC(dat$y, p_r[,1])
AUC(dat$y, p_r_a[,1])

#Checking on external data
set.seed(101)
external <- generate_data(10^5, beta, correl_mat)
x <- model.matrix(y ~ .-p, external)

e_p_sl <- predict(logit_model, external, type="response")
e_p_r <- predict(cv_ridge, x, s = "lambda.1se", type="response")
e_p_r_a <- predict(cv_ridge_acc, x, s = "lambda.1se", type="response")

#apparent performance measures
Accuracy(external$y, e_p_sl)
Accuracy(external$y, e_p_r) #also external accuracy is worse
Accuracy(external$y, e_p_r_a)

AUC(external$y, e_p_sl)
AUC(external$y, e_p_r[,1])  #Auc also worse
AUC(external$y, e_p_r_a[,1])


# Simulation --------------------------------------------------------------


n_sim <- 20 # its is rather low, but we can already see trends
n_t <- 100
n_pred <- 6
beta <- rep(0, n_pred+1)
coefs <- c(2,2,-2,-2)
beta[2:(length(coefs)+1)] <- coefs
beta[1:5]
 


correl_mat <- create_exponential_correl_mat(n_pred, 0.9)
#correl_mat <- create_covariance_matrix(n_pred, 10)

d <- generate_data(10^6 , beta, correl_mat)

ggplot(d, aes(x = p)) +
  geom_density(color = "grey15", fill = "grey20", alpha = .15) + theme_bw()
#density of probabilities
plot(density(d$p))
#checking maximum possible accuracy
Accuracy(d$y, d$p)
AUC(d$y, d$p)

log_acc <- ridge_acc <- lasso_acc <-
  app_log_acc <- app_ridge_acc <- app_lasso_acc <-  numeric(n_sim)
log_coef <- ridge_coef <- lasso_coef <- matrix(0, n_sim, n_pred)

subset_cols <- F #Only using a subset of the predictors, (didn't help either)
use_logistic <- F #When logistic cannot be fit, it should be F

set.seed(1337)

for (i in 1:n_sim){
  dat <- generate_data(n_t, beta, correl_mat)
  
  if (subset_cols){
    known_vars <- sample(n_pred, n_pred%/%2)
    selected_cols <- c(known_vars, n_pred+1, n_pred+2)
    dat <- dat[,selected_cols]
  }
  
  if (use_logistic) logist_m <- glm(y ~. -p, family = "binomial", data = dat)
  
  x <- model.matrix(y ~ .-p, dat)
  cv_ridge <- cv.glmnet(x, dat$y, family = "binomial", alpha = 0) #, type.measure = "class")
  cv_lasso <- cv.glmnet(x, dat$y, family = "binomial", alpha = 1) #, type.measure = "class")
  
  if (use_logistic) log_coef[i,] <- coef(logist_m)[-1]
  ridge_coef[i,] <- coef(cv_ridge, s = cv_ridge$lambda.1se)[c(-1,-2)]
  lasso_coef[i,] <- coef(cv_lasso, s = cv_lasso$lambda.1se)[c(-1,-2)]
  
  if (use_logistic) lg_p <- predict(logist_m, dat, type="response")
  rg_p <- predict(cv_ridge, x, type="response", s = "lambda.1se")
  ls_p <- predict(cv_ridge, x, type="response", s = "lambda.1se")
  if (use_logistic) app_log_acc[i] <- Accuracy(dat$y, lg_p)
  app_ridge_acc[i] <- Accuracy(dat$y, rg_p)
  app_lasso_acc[i] <- Accuracy(dat$y, ls_p)
  
  external <- generate_data(10^5, beta, correl_mat)
  if (subset_cols)  external <- external[,selected_cols]
  x <- model.matrix(y ~.-p, external)
  if (use_logistic) e_lg_p <- predict(logist_m, external, type="response")
  e_rg_p <- predict(cv_ridge, x, type="response", s = "lambda.1se")[,1]
  e_ls_p <- predict(cv_lasso, x, type="response", s = "lambda.1se")[,1]
  if (use_logistic) log_acc[i] <- Accuracy(external$y, e_lg_p)
  ridge_acc[i] <- Accuracy(external$y, e_rg_p)
  lasso_acc[i] <- Accuracy(external$y, e_ls_p)
}

result_df <- data.frame(log_acc, ridge_acc, lasso_acc, app_log_acc, app_ridge_acc, app_lasso_acc)
result_df$diff_ridge <- result_df$ridge_acc - result_df$log_acc
result_df$diff_lasso <- result_df$lasso_acc - result_df$log_acc
apply(result_df, 2, mean)
#both ridge and lasso are worse, although the data is
#high dimensional and there is strong collinearity

View(result_df)

coef(cv_ridge, s = cv_ridge$lambda.1se)[1:10]
e_rg_p[1:10]
which(coef(cv_lasso, s = cv_lasso$lambda.1se) != 0)




# Selecting sd and seed for sim B ---------------------------------------------
for (i in 1:5){
  set.seed(i)
  n_pred <- 100
  beta <- c(0, rnorm(n_pred, sd = 1/4))
  cm <- create_exponential_correl_mat(n_pred, .9)
  d <- generate_data(10^4, beta, cm) 
  print(paste("Acc:", Accuracy(d$y, d$p), "AUC:", AUC(d$y, d$p)))
}
#as we can see the maximum accuracy has a high variance based on 
#the generated coefficients

n_preds <- c(10, 20, 50, 100, 200)
sds <- c(1, 1/2, 1/3, 1/4, 1/5)
good_seeds <- numeric(length(n_preds))

selected_betas <- list()


for (i in 1:5){
  n_pred <- n_preds[i]; sd_ <- sds[i]
  cm <- create_exponential_correl_mat(n_pred, .9)
  seed <- factorial(10) - 2^(i+10)
  diff_acc <- 1
  while (diff_acc > .005){
    seed <- seed + 1
    set.seed(seed)
    beta <- c(0,rnorm(n_pred, sd = sd_))
    d <- generate_data(10^4, beta, cm)
    acc <- Accuracy(d$y, d$p)
    diff_acc <- abs((.8 - acc))
    #cat(round(acc, 3), " ")
  }
  good_seeds[i] <- seed
  print(acc)
  selected_betas[[paste("n_pred_", n_pred, sep = "")]] = beta
}

#estimating accuracy better
max_acc <- numeric(length(n_preds))
max_auc <- numeric(length(n_preds))

set.seed(13131313)

for (i in 1:5){
  n_pred <- n_preds[i]; sd_ <- sds[i]
  cm <- create_exponential_correl_mat(n_pred, .9)
  beta <- selected_betas[[i]]
  d <- generate_data(10^6, beta, cm)
  acc <- Accuracy(d$y, d$p);  auc_ <- AUC(d$y, d$p)
  max_acc[i] <- acc; max_auc[i] <- auc_
  cat("n_pred: ", n_pred, " ~ Acc: ", acc,
      "; AUC: ", auc_ , "\n")
}
rm(d); gc() # a little garbage collection
max_performance <- data.frame(accuracy = max_acc, AUC = max_auc)

save(selected_betas, max_performance,
     file = "Simulation/running_simulations/selected_beta_5.B.RData")
load("running_simulations/selected_beta_5.B.RData")

#without correlation
beta <- c(0, 1.2, 1.2, -1.2, -1.2)
d <- generate_data(10^6, beta)
Accuracy(d$y, d$p)
AUC(d$y, d$p)

n_preds <- c(10, 20, 50, 100, 200)
sds <- c(1, 1/2, 1/3, 1/4, 1/6)
good_seeds <- numeric(length(n_preds))

selected_betas <- list()


for (i in 1:5){
  n_pred <- n_preds[i]; sd_ <- sds[i]
  seed <- factorial(11) - 2^(i+10)
  diff_acc <- 1
  while (diff_acc > .005){
    seed <- seed + 1
    set.seed(seed)
    beta <- c(0,rnorm(n_pred, sd = sd_))
    d <- generate_data(10^4, beta)
    acc <- Accuracy(d$y, d$p)
    diff_acc <- abs((.8 - acc))
    #cat(round(acc, 3), " ")
    #cat("\r", round(acc, 3))
  }
  good_seeds[i] <- seed
  print(acc)
  selected_betas[[paste("n_pred_", n_pred, sep = "")]] = beta
}

max_acc <- numeric(length(n_preds))
max_auc <- numeric(length(n_preds))

set.seed(13131313)

for (i in 1:5){
  n_pred <- n_preds[i]
  beta <- selected_betas[[i]]
  d <- generate_data(5e5, beta)
  acc <- Accuracy(d$y, d$p);  auc_ <- AUC(d$y, d$p)
  max_acc[i] <- acc; max_auc[i] <- auc_
  cat("n_pred: ", n_pred, " ~ Acc: ", acc,
      "; AUC: ", auc_ , "\n")
}
rm(d); gc() # a little garbage collection
max_performance <- data.frame(accuracy = max_acc, AUC = max_auc)
save(selected_betas, max_performance,
     file = "../Simulation/running_simulations/selected_beta_uncor.RData")
