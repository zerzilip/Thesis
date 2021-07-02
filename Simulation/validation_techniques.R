source("performance_measures.R")
source("predictive_models.R")


stratified_samples <- function(data, k){
  
  index_0 <- which(data$y == 0); index_1 <- which(data$y == 1)
  shuffled_0 <- sample(index_0); shuffled_1 <- sample(index_1)
  folds <- list()
  for (i in 1:k){
    folds[[i]] <- c(shuffled_0[1:length(shuffled_0) %% k == i-1],
                    shuffled_1[1:length(shuffled_1) %% k == k-i])
  }
  return(folds)
}


kfold_CV <- function(data, performance_measures, models,
                     k_CV = 10, pooled = FALSE, ...){

  fold_indices <- stratified_samples(data, k_CV)
  result_length <- length(models)*length(performance_measures$names)
  results <- matrix(nrow = k_CV, ncol = result_length)
  
  pooled_data <- as.data.frame(matrix(nrow = nrow(data),
                                      ncol =  length(models) + 1))
  names(pooled_data) <- c("y", names(models))
  j = 1
  
  for (i in 1:k_CV){
    train_data <- data[-fold_indices[[i]],]
    test_data <- data[fold_indices[[i]],]
    fold_result <- numeric(0)
    
    len_test <- nrow(test_data)
    pooled_data$y[j:(j+len_test-1)] <- test_data$y
    n_model <- 1
    
    for (model in models){
      fitted_model <- model$fit(train_data)
      p <- model$predict(fitted_model, test_data)
      perf_meas <- performance_measures$calculate_all(test_data$y, p)
      fold_result <- c(fold_result, perf_meas)
      
      pooled_data[j:(j+len_test-1),(n_model+1)] <- p
      n_model <- n_model + 1
    }
    results[i,] <- fold_result
    j <- j + len_test
  }
  
  final_res <- apply(results, 2, mean, na.rm = TRUE)
  
  if (pooled){
    pooled_res <- numeric(0)
    for (i in 2:ncol(pooled_data)){
      perf_meas <- performance_measures$calculate_all(pooled_data$y, pooled_data[,i])
      pooled_res <- c(pooled_res, perf_meas)
    }
    return(list(k_CV = final_res,
                pooled_CV = pooled_res,
                pooled_data = pooled_data))
  }
  return(final_res)
}

repeated_CV <- function(data, performance_measures, models,
                        k_RCV = 10, r = 10, pooled = FALSE, ...){
  
  if (pooled){
    pooled_data <- data.frame(matrix(ncol=length(models) + 1, nrow = 0,
                      dimnames=list(NULL, c("y",names(models)))))
    result_length <- length(models)*length(performance_measures$names)
    cv_results <- matrix(nrow = r, ncol = result_length)
    for (i in 1:r){
      pooled_CV <- kfold_CV(data, performance_measures,
                            models, k_RCV, pooled = TRUE)
      
      cv_results[i,] <- pooled_CV$k_CV
      pooled_data <- rbind(pooled_data, pooled_CV$pooled_data)
      
    }
    rcv_res <- apply(cv_results, 2, mean)
    pooled_res <- numeric(0)
    for (i in 2:ncol(pooled_data)){
      perf_meas <- performance_measures$calculate_all(pooled_data$y, pooled_data[,i])
      pooled_res <- c(pooled_res, perf_meas)
    }
    return(list(rcv = rcv_res,
                pooled_rcv = pooled_res))
    
  }
  
  results <- replicate(r, kfold_CV(data, performance_measures, models, k_RCV))
  final_res <- apply(results, 1, mean, na.rm = TRUE)
  return(final_res)
  
}


bootstrap_methods <- function(data, performance_measures, models, b = 100,
                              n_no_information = 1000, ...){
  
  bootstrap_samples <- replicate(b, {
    indices <- sample(nrow(data), replace = TRUE)
    train_data <- data[indices,]
    external_data <- data[-indices,]
    
    bootstrap_apparent <- numeric(0) #for simple bootstrap
    bootstrap_test <- numeric(0) #for simple bootstrap
    bootstrap_external <- numeric(0) #for 0.632 bootstrap
    for (model in models){
      fitted_model <- model$fit(train_data)
      
      pred_apparent <- model$predict(fitted_model, train_data)
      pred_test <- model$predict(fitted_model, data)
      pred_external <- model$predict(fitted_model, external_data)
      
      apparent_pm <- performance_measures$calculate_all(train_data$y, pred_apparent)
      test_pm <- performance_measures$calculate_all(data$y, pred_test)
      external_pm <- performance_measures$calculate_all(external_data$y, pred_external)
      
      bootstrap_apparent <- c(bootstrap_apparent, apparent_pm)
      bootstrap_test <- c(bootstrap_test, test_pm)
      bootstrap_external <- c(bootstrap_external, external_pm)
    }
    
    
    optimism <- bootstrap_apparent - bootstrap_test
    return(rbind(optimism, bootstrap_external))
  })
  
  avg_results <- apply(bootstrap_samples, c(1,2), mean, na.rm = TRUE)
  optimism <- avg_results[1,]
  external_bootstrap <- avg_results[2,]
  
  apparent_pm <- numeric(0)
  no_information_pm <- numeric(0)
  
  for (model in models){
    fitted_model <- model$fit(data)
    p <- model$predict(fitted_model, data)
    pm <- performance_measures$calculate_all(data$y, p)
    no_info_pm <- performance_measures$calc_no_info_rate(n_no_information, data$y, p)
    apparent_pm <- c(apparent_pm, pm)
    no_information_pm <- c(no_information_pm, no_info_pm)
  }
  
  simple_bootstrap <- apparent_pm - optimism
  bootstrap.0632 <- 0.368*apparent_pm + 0.632*external_bootstrap
  
  R <- (external_bootstrap - apparent_pm)/(no_information_pm - apparent_pm)
  R <- ifelse((R <= 1 & R > 0), R, 0)
  w <- 0.632 / (1 - 0.368*R)
  bootstrap.0632_plus <- (1-w)*apparent_pm + w*external_bootstrap
  
  return(rbind(simple_bootstrap, bootstrap.0632, bootstrap.0632_plus))
}


