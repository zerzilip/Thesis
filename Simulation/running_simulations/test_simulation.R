rm(list=ls())

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

models_list <- list(llm = logistic_model, ridge = ridge_model, lasso = lasso_model)

seed <- 420
n_training <- 100
n_simulation <- 8
n_external <- 10^5


n_pred <- 10
beta <- rep(0, n_pred+1)
coefs <- c(2,2,-2,-2)
beta[2:(length(coefs)+1)] <- coefs
beta
#beta <- c(0, rnorm(n_pred)) /2


correl_mat <- create_exponential_correl_mat(n_pred, 0.9)
#correl_mat <- create_covariance_matrix(n_pred, 10)

d <- generate_data_from_logistic(10^5 , beta, correl_mat)
#density of probabilities
plot(density(d$p))
#checking maximum possible accuracy
performance_measures$Accuracy(d$y, d$p)

start.time <- Sys.time()
results <- run_simulation(n_training, n_simulation, n_external,
                          generate_data_from_logistic, performance_measures,
                          models_list, seed, beta = beta, cor_mat = correl_mat)

stopCluster(cl)

print(paste("It took ", round(difftime(Sys.time(), start.time, units = "secs")),
            "seconds to run."))


save(results, models_list, file = "data/cluster_test_1.RData")



