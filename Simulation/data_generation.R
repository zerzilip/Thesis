library(MASS)

generate_data_from_logistic <- function(n, beta, cor_mat = NULL, ...){
  n_pred <- length(beta) - 1 #numper of predictors
  
  x <- if (is.null(cor_mat)) matrix(rnorm(n*n_pred), n, n_pred) else
    MASS::mvrnorm(n, rep(0, n_pred), cor_mat)
  
  design_matrix <- cbind(1, x)
  
  p <- as.numeric(1/(1+exp(- design_matrix %*% beta))) #true probabilities from logit model
  y <- rbinom(n, 1, p) #drawing outcomes from Bernoulli distribution
  dat <- as.data.frame(x)
  dat$y <- y; dat$p <- p
  return(dat)
} 

create_exponential_correl_mat <- function(n_pred, gamma){
  a <- c((n_pred-1):0, 1:(n_pred-1))
  exp_mat <- sapply(n_pred:1, function(x) a[x:(x+n_pred-1)])
  gamma^exp_mat
}

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

set_intercept <- function(beta, data_generator, ratio = .5, intercept = 0, n_iter = 100){
  
  n_data <- c(10^2, 10^3, 10^4, 10^5)
  steps <- c(.1, .03, .01, .003)
  good_guess <- 0; i <- 1
  
  while (i <= length(n_data)) {
    n <- n_data[i]; step <- steps[i]
    coefs <- c(intercept, beta)
    d <- data_generator(n, coefs)
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
    coefs <- c(intercept, beta)
    d <- data_generator(10^5, coefs)
    random_ratio <- sum(d$y == 1)/10^5
    intercept = if (random_ratio < ratio) intercept + .001 else intercept - .001
    ints[i] <- intercept
  }
  
  return(ints)
}
