
generate_data <- function(n, beta, ...){
  n_pred <- length(beta) - 1 #numper of predictors
  x <- matrix(rnorm(n*n_pred), n, n_pred) #generated x
  
  design_matrix <- cbind(1, x)
  
  p <- as.numeric(1/(1+exp(- design_matrix %*% beta))) #true probabilities from logit model
  y <- rbinom(n, 1, p) #drawing outcomes from Bernoulli distribution
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


beta <- c(1, -1, 1)

n_iter <- 500
intercepts <- set_intercept(beta, generate_data, .95, 0, n_iter)

plot(intercepts, type = "l")

a <- generate_data(100000, c(mean(intercepts[100:n_iter]), 1, -1, 1))
sum(a$y == 1)
