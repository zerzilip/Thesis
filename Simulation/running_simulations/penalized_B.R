setwd("..")
source("validation_simulation.R")
source("data_generation.R")
library(doParallel, quiet = TRUE)

load("running_simulations/selected_beta_5.B.RData")

on_cluster <- Sys.info()['sysname'] == "Linux"

print(getwd())

cores <- if(on_cluster) Sys.getenv(paste("SLURM_CPUS_PER_TASK")) else 4

print(paste("Using ",cores, " cores"))

cl <- makeCluster(as.numeric(cores))
registerDoParallel(cl)

performance_measures <- create_performance_measures_list()

models_list <- list(ridge = ridge_model, lasso = lasso_model)

n_sim <- 1000 # its is rather low, but we can already see trends
n_t <- 100
n_pred <- c(10,20,50,100,200)
n_external <- 10^6

all_results <- list()

for (i in 1:length(n_pred)){
  beta <- selected_betas[[i]]
  correl_mat <- create_exponential_correl_mat(n_pred[i], 0.9)
  start.time <- Sys.time()
  results <- run_simulation(n_t, n_sim, n_external,
                            generate_data_from_logistic, performance_measures,
                            models_list, seed = 42*i, pooled = TRUE,
                            beta = beta, cor_mat = correl_mat)
  all_results[[paste("n_pred", n_pred[i], sep = "_")]] <- results
  
  print(paste("Simulation", i, "took", round(difftime(Sys.time(), start.time, units = "secs")),
              "seconds to run."))
}
stopCluster(cl)

save(all_results, file = "data/penalized_B.RData")
