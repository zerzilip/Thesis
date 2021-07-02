setwd("..")
source("validation_simulation.R")
source("data_generation.R")
library(doParallel, quiet = TRUE)

on_cluster <- Sys.info()['sysname'] == "Linux"


cores <- if(on_cluster) Sys.getenv(paste("SLURM_CPUS_PER_TASK")) else 4

print(paste("Using ",cores, " cores"))

cl <- makeCluster(as.numeric(cores))
registerDoParallel(cl)

performance_measures <- create_performance_measures_list()

models_list <- list(llm = logistic_model)

n_training <- 100
beta_slope <- c(1, -1, .5, -.5)
n_simulation <- 2000
n_external <- 10^6


class_ratio <- 1:5 / 10
len_cr <- length(class_ratio)
intercepts <- numeric(len_cr)
n_iter <- 2000

set.seed(07031997)

for (i in 1:(len_cr-1)){
  intercept_estimates <- set_intercept(beta_slope, generate_data_from_logistic,
                                      class_ratio[i], n_iter = n_iter)
  intercepts[i] <- mean(intercept_estimates[200:n_iter])
}
intercepts[len_cr] <- 0 #

all_results <- list()

for (i in 1:length(class_ratio)){
  beta <- c(intercepts[i], beta_slope)
  results <- run_simulation(n_training, n_simulation, n_external,
                            generate_data_from_logistic, performance_measures,
                            models_list, beta = beta, seed = 69*i, pooled = TRUE)
  all_results[[paste("class_ratio", class_ratio[i], sep = "_")]] <- results
}


save(all_results, intercepts, file = "data/cluster_class_ratio.RData")
