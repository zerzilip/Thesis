# setwd("~/MustMast/Thesis/Code")
source("validation_techniques.R")
library(foreach)
library(abind)


combine_results <- function(...) abind(list(...), along = 3)

run_simulation <- function(n_training, n_simulation, n_external_data,
                           data_generating, performance_measures, models,
                           seed, pooled = TRUE, packages = NULL, ...){
  set.seed(seed)
  random_seeds <- sample(1000000000, n_simulation)
  source("validation_techniques.R", local = TRUE)
 
  
  results <- foreach(i = 1:n_simulation, .combine = combine_results,
                     .packages = packages) %dopar% {
    set.seed(random_seeds[i])
    full_data <- data_generating(n_training, ...)
    
    kfold_result <- kfold_CV(full_data, performance_measures, models,
                             pooled = pooled, ...)
    RCV_results <- repeated_CV(full_data, performance_measures, models,
                               pooled = pooled,  ...)
    bootstrap_results <- bootstrap_methods(full_data, performance_measures, models, ...)
    
    EP_T <- numeric(0)
    external_data <- data_generating(n_external_data, ...)
    for (model in models){
      fitted_model <- model$fit(full_data)
      pred <- model$predict(fitted_model, external_data)
      EP_T <- c(EP_T, performance_measures$calculate_all(external_data$y, pred))
    }
    
    if(pooled){
      result <- rbind(kfold_result$k_CV, kfold_result$pooled_CV,
                      RCV_results$rcv, RCV_results$pooled_rcv, 
                      bootstrap_results, EP_T)
      rownames(result) <- c("cv", "p_cv", "rcv", "p_rcv", 
                            "SB", "0.632", "0.632+", "EP_T")
    } else {result <- rbind(kfold_result, RCV_results, bootstrap_results, EP_T)}

    result_names <- expand.grid(performance_measures$names, names(models))
    result_names <- paste(result_names$Var2, result_names$Var1, sep = "_")
    colnames(result) <- result_names
    result
  }
  return(results)
}


# run_simulation <- function(n_training, n_simulation, n_external_data,
#                            data_generating, performance_measures, models,
#                            seed, pooled = TRUE, packages = NULL, ...){
#   set.seed(seed)
#   random_seeds <- sample(1000000000, n_simulation)
#   source("validation_techniques.R", local = TRUE)
#   
#   
#   full_data <- data_generating(n_training, ...)
#   
#                     
#   kfold_result <- kfold_CV(full_data, performance_measures, models,
#                                                 pooled = pooled, ...)
#   RCV_results <- repeated_CV(full_data, performance_measures, models,
#                                                   pooled = pooled,  ...)
#   bootstrap_results <- bootstrap_methods(full_data, performance_measures, models, ...)
#                        
#   EP_T <- numeric(0)
#   external_data <- data_generating(n_external_data, ...)
#   for (model in models){
#      fitted_model <- model$fit(full_data)
#      plot(fitted_model)
#      pred <- model$predict(fitted_model, external_data)
#      EP_T <- c(EP_T, performance_measures$calculate_all(external_data$y, pred))
#   }
#                        
#   if(pooled){
#     result <- rbind(kfold_result$k_CV, kfold_result$pooled_CV,
#                     RCV_results$rcv, RCV_results$pooled_rcv, 
#                     bootstrap_results, EP_T)
#     rownames(result) <- c("cv", "p_cv", "rcv", "p_rcv", 
#                           "SB", "0.632", "0.632+", "EP_T")
#   } else {result <- rbind(kfold_result, RCV_results, bootstrap_results, EP_T)}
#   
#   result_names <- expand.grid(performance_measures$names, names(models))
#   result_names <- paste(result_names$Var2, result_names$Var1, sep = "_")
#   colnames(result) <- result_names
# 
# 
#   return(result)
# }
