setwd("..")
source("validation_simulation.R")
source("data_generation.R")
library(doParallel, quiet = TRUE)

on_cluster <- Sys.info()['sysname'] == "Linux"

print(getwd())

cores <- if(on_cluster) Sys.getenv(paste("SLURM_CPUS_PER_TASK")) else 4

print(paste("Using ",cores, " cores"))

cl <- makeCluster(as.numeric(cores))
registerDoParallel(cl)

performance_measures <- create_performance_measures_list()

models_list <- list(llm = logistic_model)

training_sample_size <- c(20, 40, 60, 80, 100, 200, 500, 1000)
beta <- c(0, 1, -1, .5, -.5)
n_simulation <- 100
n_external <- 10^6

all_results <- list()

for (i in 1:length(training_sample_size)){
  n_training <- training_sample_size[i]
  results <- run_simulation(n_training, n_simulation, n_external,
                            generate_data_from_logistic, performance_measures,
                            models_list, beta = beta, seed = 420*i, pooled = TRUE)
  all_results[[paste("sample_size", n_training, sep = "_")]] <- results
}


save(all_results, file = "data/cluster_sample_size.RData")
