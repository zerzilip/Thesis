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

n_training <- 100
original_beta <- c(0, .2, -.2, .1, -.1)
n_simulation <- 2000
n_external <- 10^6

all_results <- list()

for (i in 0:5){
  beta <- original_beta * i
  results <- run_simulation(n_training, n_simulation, n_external,
                            generate_data_from_logistic, performance_measures,
                            models_list, beta = beta, seed = 1337*i, pooled = TRUE)
  all_results[[paste("signal_strength", i, sep = "_")]] <- results
}


save(all_results, file = "data/cluster_signal_strength.RData")
