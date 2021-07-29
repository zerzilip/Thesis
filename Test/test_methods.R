setwd("~/MustMast/Thesis/Code/Simulation/")
source("validation_techniques.R")
source("data_generation.R")



models_list <- list(llm = logistic_model)
pm <- create_performance_measures_list()
beta <- c(0, 1, -1, .5, -.5)


seed <- 420
n_training <- 100
n_simulation <- 4
n_external <- 10^5

d <- generate_data_from_logistic(10^5 , beta)
#density of probabilities
plot(density(d$p))
#checking maximum possible accuracy
pm$Accuracy(d$y, d$p)


set.seed(11011906)
full_data <- generate_data_from_logistic(n_training, beta)

kfold_result <- kfold_CV(full_data, pm, models_list)
RCV_results <- repeated_CV(full_data, pm, models_list)

bootstrap_results <- bootstrap_methods(full_data, pm, models_list, b = 100)

kfold_result <- kfold_CV(full_data, pm, models_list, pooled = TRUE)
rcv_results_2 <- repeated_CV(full_data, pm, models_list, pooled = TRUE)

n_sim <- 50
glm_acc <- glm_auc <- old_acc <- old_auc <- numeric(n_sim)
set.seed(12)
for (i in 1:n_sim){
  d <- generate_data_from_logistic(100, beta)
  external <- generate_data_from_logistic(10^4, beta)
  
  for (model in models_list){
    m <- model$fit(d)
    p <- model$predict(m, external)
    old_acc[i] <- pm$Accuracy(external$y, p)
    old_auc[i] <- pm$AUC(external$y, p)
  }
  
  m_lm <- glm(y ~.-p, d, family = "binomial")
  p <- predict(m_lm, external, type = "response")
  glm_acc[i] <- pm$Accuracy(external$y, p)
  glm_auc[i] <- pm$AUC(external$y, p)
  
}
result_df <- data.frame(glm_acc, glm_auc, old_acc, old_auc)
View(result_df)
apply(result_df, 2, mean)
