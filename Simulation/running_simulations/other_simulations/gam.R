setwd("..")
source("validation_simulation.R")
source("data_generation.R")
library(doParallel, quiet = TRUE)

on_cluster <- Sys.info()['sysname'] == "Linux"

print(getwd())

cores <- if(on_cluster) Sys.getenv(paste("SLURM_CPUS_PER_TASK")) else 3

print(paste("Using ",cores, " cores"))

cl <- makeCluster(as.numeric(cores))
registerDoParallel(cl)

performance_measures <- create_performance_measures_list()

# dfs <- c(1,4,7,15)
# models_list <- list()
# for (df_ in dfs){
#   m <- list(fit = make_gam_fit(df_), predict = gam_predict)
#   models_list[[paste("gam_df_",df_,sep = "")]] <- m
# }

models_list <- list(logistic = logistic_model,
                    gam_df4 = gam_df4,
                    gam_df7 = gam_df7,
                    gam_df15 = gam_df15)

f_1 <- function(x) x^4/4-1
f_2 <- function(x) sin(x*3)*2
f_3 <- function(x) ifelse(x < 1, 1, -2*x + 3)
f_4 <- function(x) x^3/3

list_of_functions <- list(f_1, f_2, f_3, f_4)
intrcpt <- -0.5224
n_sim <- 2000
n_t <- 100
n_external <- 10^5


start.time <- Sys.time()
all_results <- run_simulation(n_t, n_sim, n_external,
                          generate_nonlinear_data, performance_measures,
                          models_list, seed = 5^6, pooled = TRUE,
                          packages = "gam",
                          list_of_func = list_of_functions,
                          intercept = intrcpt)

print(paste("Simulation took", round(difftime(Sys.time(), start.time, units = "secs")),
            "seconds to run."))

stopCluster(cl)

save(all_results, list_of_functions, intrcpt, file = "data/gam.RData")



