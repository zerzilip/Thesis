library("gam")
source("performance_measures.R")
pm <- create_performance_measures_list()

draw_function <- function(func, intervals = c(-2, 2), n = 1000){
  x <- seq(intervals[1], intervals[2], length.out = n)
  plot(x, func(x), type = "l")
}

AUC <- function(y, prob) as.numeric(pROC::auc(y, prob, direction="<", quiet = TRUE))
Accuracy <- function(y, prob, t = .5) mean(y == (prob > t))

generate_nonlinear_data <- function(n ,list_of_func, intercept = 0,
                                    limits = c(-2, 2)){
  n_pred <- length(list_of_func)
  x <- matrix(runif(n*n_pred, limits[1], limits[2]), n, n_pred)
  eta <- rep(intercept, n)
  for (i in 1:n_pred) {
    func <- list_of_func[[i]]
    eta <- eta + func(x[,i])
  }
  p <- 1/(1+exp(-eta))
  y <- rbinom(n, 1, p) #
  dat <- as.data.frame(x)
  dat$y <- y; dat$p <- p
  return(dat)
}

make_gam_formula <- function(d, df = 4){
  n_pred <- ncol(d) - 2 #p and y are not predictors
  var_names <- colnames(d)[1:n_pred]
  if (length(df) == n_pred) df <- rep(df[1], n_pred)
  formula_ <- paste("s(",var_names,", df = ", df, ")", sep = "")
  formula_ <- paste(formula_, collapse = " + ")
  formula_ <- paste("y ~", formula_)
  return(as.formula(formula_))
}

set_intercept <- function(list_of_func, data_generator, ratio = .5, intercept = 0, n_iter = 100){
  
  n_data <- c(10^2, 10^3, 10^4, 10^5)
  steps <- c(.1, .03, .01, .003)
  good_guess <- 0; i <- 1
  
  while (i <= length(n_data)) {
    n <- n_data[i]; step <- steps[i]
    d <- data_generator(n, list_of_func, intercept)
    n_1s <- sum(d$y == 1)
    random_ratio <- n_1s / n
    
    p_binom <- pbinom(n_1s, n, ratio)
    #cat("intercept:", intercept, "pbinom", p_binom, "\n")
    if (abs(p_binom - .5) < 0.25){
      good_guess <- good_guess + 1
      #cat("good_guesses:", good_guess, "\n")
      if (good_guess == 3) {i <- i + 1; good_guess <- 0}
      #cat("hallelujah", i, "\n")}
    } else {
      good_guess <- 0
      intercept = if (random_ratio < ratio) intercept + step else intercept - step
    }
  }
  
  
  ints <- numeric(n_iter)
  for (i in 1:n_iter){
    d <- data_generator(10^5, list_of_func, intercept)
    random_ratio <- sum(d$y == 1)/10^5
    intercept = if (random_ratio < ratio) intercept + .001 else intercept - .001
    ints[i] <- intercept
  }
  
  return(ints)
}


f_1 <- \(x) x^4/4-1
draw_function(f_1)
f_2 <- \(x) sin(x*3)*2
draw_function(f_2)
f_3 <- \(x) ifelse(x < 1, 1, -2*x + 3)
draw_function(f_3)
f_4 <- \(x) x^3/3
draw_function(f_4)

list_of_functions <- list(f_1, f_2, f_3, f_4)
d <- generate_nonlinear_data(10^5, list_of_functions)
plot(density(d$p))
sum(d$y) / nrow(d)
Accuracy(d$y, d$p)

intercepts <- set_intercept(list_of_functions, generate_nonlinear_data, .5, 0, 1000)
plot(intercepts, pch = 16, cex = .2)
(intrcpt <- mean(intercepts[100:1000]))
#intrcpt <- -0.5224

d <- generate_nonlinear_data(10^5, list_of_functions, intrcpt)
plot(density(d$p))
sum(d$y) / nrow(d)
Accuracy(d$y, d$p)

set.seed(11^7)
d <- generate_nonlinear_data(100, list_of_functions, intrcpt)
external <- generate_nonlinear_data(10^5, list_of_functions, intrcpt)

m_lm <- glm(y ~.-p, d, family = "binomial")
summary(m_lm)
p <- predict(m_lm, d, type = "response")
Accuracy(d$y, p)

#m_gam <- gam_fit_df4(d)

m_gam <- gam(make_gam_formula(d, 7), data = d, family = "binomial")
p <- predict(m_gam, external, type = "response")
Accuracy(external$y, p)
plot(m_gam)

p <- predict(m_gam, d, type = "response")
pm$cox_calibration(d$y, p)


n_sim <- 100
glm_acc <- glm_auc <- gam_acc <- gam_auc <- numeric(n_sim)

for (i in 1:n_sim){
  d <- generate_nonlinear_data(100, list_of_functions, intrcpt)
  external <- generate_nonlinear_data(10^4, list_of_functions, intrcpt)
  
  m_lm <- glm(y ~.-p, d, family = "binomial")
  p <- predict(m_lm, external, type = "response")
  glm_acc[i] <- Accuracy(external$y, p)
  glm_auc[i] <- AUC(external$y, p)
  
  m_gam <- gam(make_gam_formula(d, 7), data = d, family = "binomial")
  p <- predict(m_gam, external, type = "response")
  gam_acc[i] <- Accuracy(external$y, p)
  gam_auc[i] <- AUC(external$y, p)
}
result_df <- data.frame(glm_acc, glm_auc, gam_acc, gam_auc)
apply(result_df, 2, mean)
View(result_df)
