library(ROCR)
library(ggplot2)
library(MASS)

logit <- function (p) log(p/(1 - p))

cox_calibration <- function(y, prob){
  dat <- data.frame(e = prob, o = y)
  dat$e[dat$e < 0.0000000001] = 0.0000000001
  dat$e[dat$e > 0.9999999999] = 0.9999999999
  dat$logite <- logit(dat$e)
  
  mfit = glm(formula = o~logite, 
             family = binomial(link = "logit"), dat)
  
  return(list(slope = as.numeric(mfit$coefficients[2]),
              intercept = as.numeric(mfit$coefficients[1])))
}

generate_data <- function(n, beta){
  n_pred <- length(beta) - 1 #numper of predictors
  x <- matrix(rnorm(n*n_pred), n, n_pred) #generated x
  
  design_matrix <- cbind(1, x)
  
  p <- as.numeric(1/(1+exp(- design_matrix %*% beta))) #true probabilities from logit model
  y <- rbinom(n, 1, p) #drawing outcomes from Bernoulli distribution
  dat <- as.data.frame(x)
  dat$y <- y; dat$p <- p
  return(dat)
}


make_long_format <- function(list_of_df){
  n_df <- length(list_of_df)
  n_col <- ncol(list_of_df[[1]])
  n_row <- nrow(list_of_df[[1]])
  result_len <- n_df * n_col * n_row
  result <- data.frame(sample_size = character(result_len),
                       estimate = character(result_len),
                       value = numeric(result_len))
  df_names <- names(list_of_df)
  
  i = 1
  for (df_name in df_names){
    indices <- i:(i+n_col * n_row - 1)
    result$sample_size[indices] <- rep(df_name, length(indices))
    df <- stack(list_of_df[[df_name]])
    result$estimate[indices] <- as.character(df$ind)
    result$values[indices] <- df$values
    i <- i + n_col * n_row
  }
  result$sample_size <- factor(result$sample_size, levels = df_names, ordered = TRUE)
  return(result)
}


make_empty_data.frame <- function(n) data.frame(theoretical = numeric(n),
                                                conditional = numeric(n),
                                                apparent = numeric(n))

set.seed(6042021)
n_simulation <- 10000
sample_sizes <- c(25, 50, 100, 250, 1000)


AUC_results <- list()
cox_slope_results <- list()
cox_intercept_results <- list()

beta <- c(-0.3, 1, -1, .3) 

# It takes 5 minutes to run, loading the results might be a better option
for (n in sample_sizes){
  
  df_AUC <- make_empty_data.frame(n_simulation)
  df_CS <- make_empty_data.frame(n_simulation)
  df_CI <- make_empty_data.frame(n_simulation)
  
  for (i in 1:n_simulation){
    full_data <- generate_data(n, beta)
    logit_model <- glm(y ~. -p, family = "binomial", data = full_data)
    predictions <- predict(logit_model, full_data, type="response")
    
    cal_slp <- cox_calibration(full_data$y, predictions)
    df_CS$apparent[i] <- cal_slp$slope
    df_CI$apparent[i] <- cal_slp$intercept
    df_AUC$apparent[i] <- pROC::auc(full_data$y, predictions, quiet = TRUE)
    
    cal_slp <- cox_calibration(full_data$y, full_data$p)
    df_CS$theoretical[i] <- cal_slp$slope
    df_CI$theoretical[i] <- cal_slp$intercept
    df_AUC$theoretical[i] <- pROC::auc(full_data$y, full_data$p, quiet = TRUE)
    
    new_data <- generate_data(10000, beta)
    pred_new <- predict(logit_model, new_data, type="response")

    cal_slp <- cox_calibration(new_data$y, pred_new)
    df_CS$conditional[i] <- cal_slp$slope
    df_CI$conditional[i] <- cal_slp$intercept
    df_AUC$conditional[i] <- pROC::auc(new_data$y, pred_new, quiet = TRUE)
  }
  
  ss_name <- paste("n_", n, sep = "")
  AUC_results[[ss_name]] <- df_AUC
  cox_slope_results[[ss_name]] <- df_CS
  cox_intercept_results[[ss_name]] <- df_CI
  
}



save(AUC_results, cox_slope_results, cox_intercept_results,
     file = "perf_meas_simulation_results_n10000.RData")

load("perf_meas_simulation_results_n10000.RData")

sum(cox_slope_results$n_25$theoretical>10)

cox_slope_results$n_25$theoretical[cox_slope_results$n_25$theoretical>10] <- 0


long_AUC <- make_long_format(AUC_results)
long_slope <- make_long_format(cox_slope_results)
long_intercept <- make_long_format(cox_intercept_results)

ggplot(long_AUC, aes(x=sample_size, y=values, fill=estimate)) + 
  geom_boxplot() + ggtitle("AUC")
ggplot(long_slope, aes(x=sample_size, y=values, fill=estimate)) + 
  geom_boxplot() + ggtitle("Cox slope")
ggplot(long_intercept, aes(x=sample_size, y=values, fill=estimate)) + 
  geom_boxplot() + ggtitle("Cox intercept")

lapply(AUC_results, function(x) apply(x, 2, mean))
lapply(AUC_results, cor)

lapply(cox_slope_results, function(x) apply(x, 2, mean))
lapply(cox_slope_results, cor)
