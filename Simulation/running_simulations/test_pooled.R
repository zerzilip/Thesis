source("validation_techniques.R")
source("data_generation.R")

library(foreach)
library(abind)
library(doParallel, quiet = TRUE)

on_cluster <- Sys.info()['sysname'] == "Linux"

print(getwd())

cores <- if(on_cluster) Sys.getenv(paste("SLURM_CPUS_PER_TASK")) else 4

print(paste("Using ",cores, " cores"))

cl <- makeCluster(as.numeric(cores))
registerDoParallel(cl)

performance_measures <- create_performance_measures_list()

models <- list(llm = logistic_model)

set.seed(420)
n_training <- 100
beta <- c(0, 1, -1, .5, -.5)
n_simulation <- 200
n_external_data <- 10^5



start.time <- Sys.time()


combine_results <- function(...) abind(list(...), along = 3)

  
source("validation_techniques.R", local = TRUE)
results <- foreach(i = 1:n_simulation, .combine = combine_results) %dopar% {
  
    full_data <- generate_data_from_logistic(n_training, beta)
    
    kfold_result <- kfold_CV(full_data, performance_measures, models, pooled = TRUE)
    RCV_results <- repeated_CV(full_data, performance_measures, models, pooled = TRUE)
    bootstrap_results <- bootstrap_methods(full_data, performance_measures, models)
    
    conditional_pm <- numeric(0)
    external_data <- generate_data_from_logistic(n_external_data, beta)
    for (model in models){
      fitted_model <- model$fit(full_data)
      pred <- model$predict(fitted_model, external_data)
      conditional_pm <- c(conditional_pm,
                          performance_measures$calculate_all(external_data$y, pred))
    }
    result <- rbind(kfold_result$k_CV, kfold_result$pooled_CV,
                    RCV_results$rcv, RCV_results$pooled_rcv, 
                    bootstrap_results, conditional_pm)
    rownames(result) <- c("cv", "p_cv", "rcv", "p_rcv", 
                          rownames(bootstrap_results), "conditional")
    
    result_names <- expand.grid(performance_measures$names, names(models))
    result_names <- paste(result_names$Var2, result_names$Var1, sep = "_")
    colnames(result) <- result_names
    result
  }


print(paste("It took ", round(Sys.time() - start.time), "seconds to run."))



save(results, file = "data/pool_test.RData")


source("evaluating_results.R")
plot_boxplot(results, 1)
plot_boxplot(results, 1, T)
plot_boxplot(results, 2)
plot_boxplot(results, 2, T)
plot_boxplot(results, 3, val_tech = c(2, 4, 5, 6, 7))
plot_boxplot(results, 3, T, val_tech = c(2, 4, 5, 6, 7))
plot_boxplot(results, 4, val_tech = c(2, 4, 5, 6, 7))
plot_boxplot(results, 4, T, val_tech = c(2, 4, 5, 6, 7))
