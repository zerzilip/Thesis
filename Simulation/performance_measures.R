create_performance_measures_list <- function(){
  
  MSE <- function(y, prob, ...) mean((y-prob)^2)
  AUC <- function(y, prob, ...) if (all(1 == y) | all(0 == y)){NA
  }else as.numeric(pROC::auc(y, prob, direction="<", quiet = TRUE))
  Accuracy <- function(y, prob, t = .5, ...) mean(y == (prob > t))

  logit <- function (p) log(p/(1 - p))
  
  cox_calibration <- function(y, prob, ...){
    
    #check if separation occurs.
    if (max(prob[y == 0]) < min(prob[y == 1])){
      return(list(slope = NA, intercept = NA))
    }
    
    #transform extreme predictions
    dat <- data.frame(e = prob, o = y)
    dat$e[dat$e < 0.0000000001] = 0.0000000001
    dat$e[dat$e > 0.9999999999] = 0.9999999999
    dat$logite <- logit(dat$e)
    
    mfit = glm(formula = o~logite, 
               family = binomial(link = "logit"), dat)
    
    return(list(slope = as.numeric(mfit$coefficients[2]),
                intercept = as.numeric(mfit$coefficients[1])))
  }

  calculate_all <- function(y, prob, t = .5){
    perf_meas <- numeric(5)
    
    perf_meas[1] <- Accuracy(y, prob, t)
    perf_meas[2] <- AUC(y, prob)
    cox_calib <- cox_calibration(y, prob)
    perf_meas[3] <- cox_calib$slope
    perf_meas[4] <- cox_calib$intercept
    perf_meas[5] <- MSE(y, prob)
    return(perf_meas)
  }
  perf_meas_names <- c("Accuracy", "AUC", "Cox_slope", "Cox_intercept", "MSE")
  
  no_information_measure <- function(n, y, prob){
    measures <- replicate(n, calculate_all(y, sample(prob)))
    return(apply(measures, 1, mean))
  }
  
  return_list <- list(Accuracy = Accuracy,
                      MSE = MSE,
                      AUC = AUC,
                      cox_calibration = cox_calibration,
                      calculate_all = calculate_all,
                      calc_no_info_rate = no_information_measure,
                      names = perf_meas_names)
  return(return_list)
}


# 
# MSE_calc <- function(y, prob, ...) mean((y-prob)^2)
# MSE_no_information <- 1
# MSE <- list(calc = MSE_calc, no_info = MSE_no_information, names = "MSE")
# 
# AUC_calc <- function(y, prob, ...) as.numeric(pROC::auc(y, prob, quiet = TRUE))
# AUC_no_information <- 0.5
# AUC <- list(calc = AUC_calc, no_info = AUC_no_information, names = "AUC")
# 
# logit <-
#   function (p)
#     log(p/(1 - p))
# 
# cox_calibration_calc <- function(y, prob, ...){
#   dat <- data.frame(e = prob, o = y)
#   dat$e[dat$e < 0.0000000001] = 0.0000000001
#   dat$e[dat$e > 0.9999999999] = 0.9999999999
#   dat$logite <- logit(dat$e)
#   
#   mfit = glm(formula = o~logite, 
#              family = binomial(link = "logit"), dat)
#   
#   return(list(slope = as.numeric(mfit$coefficients[2]),
#               intercept = as.numeric(mfit$coefficients[1])))
# }
# cox_no_information <- 1
# cox_calibration <- list(calc = cox_calibration_calc, no_info =cox_no_information,
#                         names = c("cox_slope", "cox_intercept"))

