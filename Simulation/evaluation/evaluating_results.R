library(ggplot2)
library(grid)

red = "#db3737"
orange = "#e6a23c"
blue = "#3a7cc7"
purple = "#8131b0"

val_labels <- c("CV", "PCV", "RCV", "PRCV", "OB", "0.632", "0.632+")
val_cols <- c("#b7daed", "#6eb8e0", "#c8b2ed", "#915ee6", "#ffe570", "#ffc46b", "#ff756b")

plot_boxplot <- function(result_array, perf_meas,
                         conditional = FALSE, val_tech = NULL){
  len_val_tech <- dim(result_array)[1]
  if (is.null(val_tech)) val_tech = 1:(len_val_tech-1)
  
  results_df <- as.data.frame(t(result_array[c(val_tech, len_val_tech),perf_meas,]))
  col_conditional <- ncol(results_df)
  p_title <- dimnames(result_array)[[2]][perf_meas]
  
  if (conditional){
    
    conditional_pm <- results_df - results_df[,col_conditional]
    conditional_pm <- conditional_pm[,-col_conditional]
    p <- ggplot(stack(conditional_pm), aes(x = ind, y = values))+
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype ="dashed", color = "red") +
      ggtitle(paste("conditional", p_title))
    
  } else {
    p <- ggplot(stack(results_df), aes(x = ind, y = values)) +
      geom_boxplot() + 
      geom_hline(yintercept = mean(results_df[,col_conditional]),
                linetype ="dashed", color = "red") +
      ggtitle(p_title)
  
  }
  return(p)
}

plot_grouped_boxplot <- function(result_list, perf_meas, color = "#231f70",
                                 var_names = NULL, val_tech = NULL, legend_labs = NULL, 
                                 legend_title = "", conditional = FALSE){
  
  val_labels <- c("CV", "PCV", "RCV", "PRCV", "BS", "0.632", "0.632+", expression(EP[T]))
  
  len_val_tech <- dim(result_list[[1]])[1]
  if (is.null(val_tech)) val_tech <- 1:(len_val_tech-1)
  if (is.null(var_names)) var_names <- paste("V", 1:length(result_list), sep = "")
  
  long_df <- data.frame(values = numeric(0), ind = character(0), sim_type = character(0))
  
  for (i in 1:length(result_list)){
    result_array <- result_list[[i]]
    result_df <- as.data.frame(t(result_array[c(val_tech, len_val_tech), perf_meas,]))
    col_conditional <- ncol(result_df)
    
    if(conditional) {
      result_df <- result_df - result_df[,col_conditional] 
      result_df <- result_df[,-col_conditional]
      } else result_df <- result_df - mean(result_df[,col_conditional], na.rm = TRUE)
    
    long_format <- stack(result_df)
    long_format$sim_type <- var_names[i]
    long_df <- rbind(long_df, long_format)
  }
  
  long_df$sim_type <- factor(long_df$sim_type, levels = var_names)
  
  alpha_values <- as.integer(seq(50, 240, length.out = length(result_list)))
  alpha_values <- as.hexmode(alpha_values)
  
  
  p <- ggplot(long_df, aes(x = ind, y = values, fill = sim_type)) +
    geom_boxplot(color = "gray34", outlier.alpha = .1, outlier.colour = color) +
    xlab("") + ylab("") + theme_bw() + theme(legend.title.align=0.5) +
    scale_x_discrete(labels = val_labels[c(val_tech, len_val_tech)]) +
    geom_hline(yintercept=0, linetype = "dashed", color = "gray20")
  
  if (is.null(legend_labs))
    p <- p + scale_fill_manual(values = paste(color, alpha_values, sep = ""),
                               name = legend_title) else
    p <- p + scale_fill_manual(values = paste(color, alpha_values, sep = ""),
                            name = legend_title, labels = legend_labs)
  # 
  # if (position == "m") p <- p + theme(axis.text.x = element_blank(),
  #                                     axis.ticks.x = element_blank(),
  #                                     plot.margin=unit(c(-0.1, .3, -0.1, .3), "cm")) else
  #   if (position == "t") p <- p + theme(axis.text.x = element_blank(),
  #                                       axis.ticks.x = element_blank(),
  #                                       plot.margin=unit(c(.2, .3, -0.3, .3), "cm")) else
  #   if (position == "b") p <- p + theme(plot.margin=unit(c(-0.3, .3, .2, .3), "cm"))
                                 
    #theme(legend.justification = "bottom")
  return(p)
}


calculate_EP <- function(result_array){
  sapply(result_array, function(x) apply(x["EP_T",,], 1, mean, na.rm = T))
}

plot_EP <- function(result_array, sim_vars, col, perf_meas = 1:4,
                    pm_names = c("accuracy", "AUC", "CS", "CI"),
                    no_scale = FALSE){
  EP <- calculate_EP(result_array)
  EP <- EP[perf_meas,]
  df <- stack(as.data.frame(EP))
  df$ind <- rep(sim_vars, each = nrow(EP))
  df$pm <- factor(rep(pm_names, ncol(EP)), levels = pm_names)
  if (no_scale) df$ind <- factor(df$ind, levels =  sim_vars)
  
  colnames(df) <- c("values", "simvar", "PM")
  p <- ggplot(df, aes(x = simvar, y = values, color = PM, group = PM))+
    geom_line(alpha = .7, size = 1.1) + theme_bw() +
    scale_color_manual(values = col)
  
  if (!no_scale) p <- p + scale_x_continuous(breaks = sim_vars, minor_breaks = NULL,
                                             labels = sim_vars)
  
  return(p)
}


plot_correlations <- function(result_list, perf_meas, sim_vars, col = NULL,
                              val_labels = NULL, val_methods = NULL,
                              no_scale = FALSE){
  
  len_val_tech <- dim(result_list[[1]])[1]
  if (is.null(val_methods)) val_methods <- 1:(len_val_tech-1)
  val_names <- dimnames(result_list[[1]])[[1]][val_methods]
  
  df_cor <- data.frame(Validation = character(0), correlation = numeric(0), sim_type = numeric(0))
  
  for (i in 1:length(sim_vars)){
    result_array <- t(result_list[[i]][,perf_meas,])
    
    temp_df <- data.frame(Validation = val_names,
                          correlation = cor(result_array[,val_methods],
                                            result_array[,len_val_tech], 
                                            use="complete.obs"),
                          sim_type = sim_vars[i])
    df_cor <- rbind(df_cor, temp_df)
  }
  
  df_cor$Validation <- factor(df_cor$Validation, levels = val_names)
  if (no_scale) df_cor$sim_type <- factor(df_cor$sim_type, levels = sim_vars)
  
  p <- ggplot(df_cor, aes(x = sim_type, y = correlation,
                          color = Validation, group = Validation)) +
    geom_line(size=1.05) + theme_bw()
  
  if (!no_scale) p <- p + scale_x_continuous(breaks = sim_vars, minor_breaks = NULL,
                                             labels = sim_vars)
  
  val_labels <- if(is.null(val_labels)) val_names else val_labels[val_methods]

  if(!is.null(col)) p <-  p + scale_color_manual(name = "Validation technique",
                                                 values = col[val_methods],
                                                 labels = val_labels)
  
  return(p)
}


plot_estimates_and_EP <- function(result_array, perf_meas, val_method,
                                  col = "blue", add_correlation = FALSE){
  conditional_col <- dim(result_array)[1]
  compare_array <- t(result_array[c(val_method, conditional_col), perf_meas,])
  compare_df <- as.data.frame(compare_array)
  
  cn <- colnames(compare_df)
  x_name <- cn[1]; y_name <- cn[2]
  
  correl <- cor(compare_df[,1], compare_df[,2], use="complete.obs")
  lbl <- paste("Cor:", as.character(round(correl, 3)))
  print(lbl)
  
  p <- ggplot(compare_df, aes_string(ensym(x_name), y_name))+
    geom_point(color = col, alpha = 0.2) + theme_bw()
  
  if (add_correlation){
    
    print(p)
    x <- readline("Please enther the coordinates where correlation should be displayed. \n x:") 
    y <- readline("y:")
    x <- as.numeric(x); y <- as.numeric(y)
    
    
    p <- p + annotate("text", x, y, label = lbl) 
  }
  return(p)
}


calculate_overall <- function(result_array, conditional, deviation = TRUE){
  func <- if (deviation) function(x, ...) sqrt(mean(x^2, na.rm = T)) else mean
  conditional_col <- dim(result_array)[1]
  
  differ <- array(NA, dim =  dim(result_array))
  
  for (i in 1:dim(result_array)[2]){
    EP_T <-result_array[conditional_col,i,]
    differ[,i,] <- if(conditional) result_array[,i,] - EP_T else
      result_array[,i,] - mean(EP_T, na.rm = T)
  }
  
  result <- apply(differ, c(1,2), func, na.rm = TRUE)
  colnames(result) <- dimnames(result_array)[[2]]
  rownames(result) <- dimnames(result_array)[[1]]
  
  res <- if(conditional) result[-conditional_col,] else result
  return(res)
}


calculate_bias_or_rmse <- function(result_list, perf_meas, rmse = T, 
                                   sim_vars = NULL, val_methods = NULL,
                                   conditional = F, long = F){
  len_val_tech <- dim(result_list[[1]])[1]
  if (is.null(val_methods)) val_methods <- 1:(len_val_tech-1)
  val_names <- dimnames(result_list[[1]])[[1]][val_methods]
  if (is.null(sim_vars)) sim_vars <- paste("4.X.", 1:length(result_list), sep = "")
  #  m <- if (long) data.frame(value, val_method, sim_type) else matrix(NA, 0, len_val_tech-1)
  m <- matrix(NA, 0, length(val_methods))
  colnames(m) <- val_names
  
  for (res in result_list){
    b <- calculate_overall(res, conditional, rmse)
    m <- rbind(m, b[val_methods,perf_meas])
  }
  
  rownames(m) <- sim_vars
  
  if (long) {
    df <- stack(as.data.frame(m))
    df$st <- rep(sim_vars, ncol(m))
    colnames(df) <- c("values", "validation", "sim_type")
  } 
  if (long) return(df) else return(m)
}

plot_comparison <- function(result_list, perf_meas,  sim_vars, rmse = T,
                            col = NULL, val_labels = NULL, val_methods = NULL,
                            conditional = T, no_scale = FALSE,
                            legend_title = "Validation technique"){
  
  len_val_tech <- dim(result_list[[1]])[1]
  if (is.null(val_methods)) val_methods <- 1:(len_val_tech-1)
  val_names <- dimnames(result_list[[1]])[[1]][val_methods]
  
  
  df <- calculate_bias_or_rmse(result_list, perf_meas, rmse,
                               sim_vars, val_methods, conditional,
                               long = T)
  
  if (no_scale) df$sim_type <- factor(df$sim_type, levels = sim_vars)
  
  p <- ggplot(df, aes(x = sim_type, y = values, col = validation, group = validation)) +
    geom_line(size = 1.1) + 
    theme_bw()
  
  if (!no_scale) p <- p + scale_x_continuous(breaks = sim_vars, minor_breaks = NULL,
                                             labels = sim_vars)
  
  val_labels <- if(is.null(val_labels)) val_names else val_labels[val_methods]
  
  if(!is.null(col)) p <-  p + scale_color_manual(name = legend_title,
                                                 values = col[val_methods],
                                                 labels = val_labels)
  p <- if(rmse) p + ylab("RMSE") else p + ylab("bias")
  
  if (!rmse) p <- p + geom_hline(yintercept=0, linetype = "dashed", color = "gray20")
  
  return(p)
}

extract_limits <- function(...){
  plots <- list(...)
  y_range <- sapply(plots, \(x) ggplot_build(x)$layout$panel_scales_y[[1]]$range$range)
  return(c(min(y_range[1,]), max(y_range[2,])))
}

align_plots <- function(...){
  y_limits <- extract_limits(...)
  plots <- list(...)
  aligned_plots <- list()
  for (i in 1:length(plots)){
    aligned_plots[[i]] <- plots[[i]] + coord_cartesian(ylim = y_limits)
  }
  return(aligned_plots)
}


if (sys.nframe() == 0){
  print("test")
}

