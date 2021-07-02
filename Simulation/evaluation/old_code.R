library(gridExtra)
library(ggplot2)
library(cowplot)
library(xtable)

setwd("..")
source("evaluation/evaluating_results.R")
source("data_generation.R")
load("data/cluster_signal_strength.RData")
signal_strength <- all_results
dimnames(signal_strength$signal_strength_0)
load("data/cluster_sample_size.RData")
training_size <- all_results
load("data/cluster_class_ratio.RData")
class_ratio <- all_results


ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
red = "#db3737"
orange = "#e6a23c"
blue = "#3a7cc7"
purple = "#8131b0"




p_cor1 <- plot_correlations(training_size, 1, ts_vars, val_cols, NULL, c(1,3,5,6,7), log_scale =  T) +
  xlab("Training sample size") + ggtitle("Accuracy (4.B)") +
  geom_point(aes(100, 0.314), shape = 4, col = "black", size = 4) +
  theme(legend.position = "none")
p_cor2 <- plot_correlations(class_ratio, 2, cr_vars, val_cols) +
  xlab("Class ratio") + ggtitle("AUC (4.C)") +
  geom_point(aes(0.3, 0.122), shape = 4, col = "black", size = 4) +
  theme(legend.position = "none")
p_cor3 <- plot_correlations(signal_strength, 3, ss_vars, val_cols, val_labels) +
  xlab("Signal strength") + ggtitle("CS (4.A)") +
  geom_point(aes(0.6, -0.645), shape = 4, col = "black", size = 4)
# + theme(legend.position = "none")



plot_grid(p_scatt1, p_scatt2, p_scatt3, p_cor1, p_cor2, p_cor3, nrow = 2, ncol = 3)

#custom_theme




xl_ss <- xlab("Signal strength") 
xl_ts <- xlab("Training size")
xl_cl <- xlab("Class ratio")
no_legend <- theme(legend.position = "none")
no_x <- theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.text.x=element_blank())
no_y <- theme(legend.position = "none",
              axis.title.y=element_blank(),
              axis.text.y=element_blank())
no_xy <-theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank())


cc <- coord_cartesian(ylim = c(-0.005, .03))
(p_1_acc_bias <- plot_comparison(signal_strength, 1, ss_vars,
                                 F, val_cols, val_labels, c(1,3,5,6,7)) +
    cc + no_x + xlab(""))
(p_2_acc_bias <- plot_comparison(training_size, 1, ts_vars, F,
                                 val_cols, val_labels, c(1,3,5,6,7), log_scale = T) +
    cc + no_xy + xlab("") + ylab(""))

(p_3_acc_bias <- plot_comparison(class_ratio, 1, cr_vars,
                                 F, val_cols, val_labels, c(1,3,5,6,7)) +
    cc + no_xy + xlab("") + ylab(""))

cc <- coord_cartesian(ylim = c(0.015, .12))
(p_1_acc_rmse <- plot_comparison(signal_strength, 1, ss_vars, T,
                                 val_cols, val_labels,  c(1,3,5,6,7)) +
    xl_ss + cc + no_legend)
(p_2_acc_rmse <- plot_comparison(training_size, 1, ts_vars, T,
                                 val_cols, val_labels, c(1,3,5,6,7), log_scale = T) +
    xl_ts + cc+ no_y + ylab(""))
(p_3_acc_rmse <- plot_comparison(class_ratio, 1, cr_vars, T, val_cols,
                                 val_labels, c(1,3,5,6,7)) +
    xl_cl + cc + no_y + ylab(""))

pg <- plot_grid(p_1_acc_bias, p_2_acc_bias, p_3_acc_bias,
                p_1_acc_rmse, p_2_acc_rmse, p_3_acc_rmse,
                nrow = 2, ncol = 3, align = "vh")

legend_b <- get_legend(
  p_1_acc_bias + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)


png('../../Figures/accuracy_br.png', width = 22, height = 15, units = 'cm', res = 400)
plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .05))
dev.off()
