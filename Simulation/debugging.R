source("evaluation/evaluating_results.R")
source("validation_simulation.R")
source("data_generation.R")
load("data/cluster_signal_strength.RData")

plot_boxplot(all_results$signal_strength_1, 2)
#What is up with CV?
which(all_results$signal_strength_1[7,1,] < -1)
#lets see the first one

#function inputs

n_training <- 100; n_simulation <- 1000; n_external_data <- 10^6
data_generating <- generate_data_from_logistic
performance_measures <- create_performance_measures_list()
models <- list(llm = logistic_model)
beta <- c(0, .2, -.2, .1, -.1)
pooled = TRUE

#Setting seed
set.seed(1337)
random_seeds <- sample(1000000000, n_simulation)
set.seed(random_seeds[8])


full_data <- data_generating(n_training, beta)

kfold_result <- kfold_CV(full_data, performance_measures, models,
                         pooled = pooled)
RCV_results <- repeated_CV(full_data, performance_measures, models,
                           pooled = pooled)
bootstrap_results <- bootstrap_methods(full_data, performance_measures, models)

EP_T <- numeric(0)
external_data <- data_generating(n_external_data, beta)
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


