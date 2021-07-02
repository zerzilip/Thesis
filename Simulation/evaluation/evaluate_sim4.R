#library(gridExtra)
library(ggplot2)
library(cowplot)
library(xtable)


setwd("..")
source("evaluation/evaluating_results.R")
source("data_generation.R")
source("performance_measures.R")
load("data/cluster_signal_strength.RData")
signal_strength <- all_results
dimnames(signal_strength$signal_strength_0)
load("data/cluster_sample_size.RData")
training_size <- all_results
load("data/cluster_class_ratio.RData")
class_ratio <- all_results


ss_vars <- 0:5 / 5
ts_vars <- c(20, 40, 60, 80, 100, 200, 500, 1000)
cr_vars <- 1:5 / 10

#Visualizing data

default_beta <- c(0, 1, -1, .5, -.5)
large_dataset <- generate_data_from_logistic(10^7, default_beta)

p <- ggplot(large_dataset, aes(x = p)) +
  geom_density(color = "grey15", fill = "grey20", alpha = .15) + theme_bw()
p


png('../../Figures/p_dist.png', width = 10, height = 6, units = 'cm', res = 400)
p
dev.off()

pm <- create_performance_measures_list()
#maximum achievable accuracy and AUC
pm$Accuracy(large_dataset$y, large_dataset$p)
pm$AUC(large_dataset$y, large_dataset$p)

rm(large_dataset)
gc()

one_iter <- signal_strength$signal_strength_5[,1:4,1]
xtable(one_iter, digits = 3)
#First, let's see the bias and standard deviation of EP and EP_T



calculate_overall(signal_strength$signal_strength_5, F, deviation = F)

ep_t_ts <- mean(training_size$sample_size_100[8,1,])
ep_cv <- mean(training_size$sample_size_100[1,1,])
p_scatt1 <- plot_estimates_and_EP(training_size$sample_size_100, 1, 1, red) +
  geom_vline(xintercept=ep_t_ts, color = "gray20") +
  geom_vline(xintercept=ep_cv, linetype="dashed", color = "gray20") +
#  geom_abline(intercept = 0, slope = 1, linetype = 3) +
#  annotate("text", .71, .625, label = "EP", color = "gray20") +
#  annotate("text", .745, .625, label = "EP[CV]", color = "gray20", parse = T) +
  ggtitle("Accuracy (4.B.5)") + 
  xlab("CV") + ylab(expression(EP[T]))  
 

ep_t_cr <- mean(class_ratio$class_ratio_0.3[8,2,])
ep_prcv <- mean(class_ratio$class_ratio_0.3[4,2,])
p_scatt2 <- plot_estimates_and_EP(class_ratio$class_ratio_0.3, 2, 4, orange) +
  geom_vline(xintercept=ep_t_cr, color = "gray20") +
  geom_vline(xintercept=ep_prcv, linetype="dashed", color = "gray20") +
  ggtitle("AUC (4.C.3)") + 
  xlab("PRCV") + ylab("")  
 
ep_t_ss <- mean(signal_strength$signal_strength_3[8,3,])
ep_0632 <- mean(signal_strength$signal_strength_3[6,3,])
p_scatt3 <- plot_estimates_and_EP(signal_strength$signal_strength_3, 3, 6, blue) + 
  geom_vline(aes(xintercept=ep_t_ss, linetype = "ep"), color = "gray20") +
  geom_vline(aes(xintercept=ep_0632, linetype = "epv"), color = "gray20") +
  scale_linetype_manual(name = "", values = c(ep = "solid", epv = "dashed"),
                        labels = c(expression(EP), expression(EP[V]))) +
  ggtitle("CS (4.A.3)") +
  xlab("0.632") + ylab("")
  

ep_ci <- mean(signal_strength$signal_strength_2[8,4,])
epv_ci <- mean(signal_strength$signal_strength_2[5,4,])
p_scatt4 <- plot_estimates_and_EP(signal_strength$signal_strength_2, 4, 5, purple) + 
  geom_vline(aes(xintercept=ep_ci, linetype = "ep"), color = "gray20") +
  geom_vline(aes(xintercept=epv_ci, linetype = "epv"), color = "gray20") +
  scale_linetype_manual(name = "", values = c(ep = "solid", epv = "dashed"),
                        labels = c(expression(EP), expression(EP[V]))) +
  ggtitle("CI (4.A.4)") +
  xlab("OB") + ylab("")

p_scatt3 <- p_scatt3 + theme(legend.position = "none")

png('../../Figures/scatter_2.png', width = 22, height = 5, units = 'cm', res = 400)
plot_grid(p_scatt1, p_scatt2, p_scatt3, p_scatt4, ncol = 4, rel_widths = c(1,1,1,1.4),labels = "AUTO")
dev.off()

#plot_estimates_and_EP(training_size$sample_size_20, 1, 1, red, T)
#plot_estimates_and_EP(training_size$sample_size_20, 3, 5, blue)




plot_correlations(signal_strength, 1, ss_vars,  val_cols, val_labels, c(1,3,5,6,7))+
  xlab("Signal strength") + ggtitle("Accuracy") 
plot_correlations(training_size, 1, ts_vars, val_cols, val_labels, c(1,3,5,6,7), no_scale= T) +
  xlab("Training sample size") + ggtitle("Accuracy")
plot_correlations(class_ratio, 1, cr_vars, val_cols, val_labels, c(1,3,5,6,7)) +
  xlab("Class ratio") + ggtitle("Accuracy") 

plot_correlations(signal_strength, 2, ss_vars, val_cols, val_labels) +
  xlab("Signal strength") + ggtitle("AUC")
plot_correlations(training_size, 2, ts_vars, val_cols, no_scale =  T) +
  xlab("Training sample size") + ggtitle("AUC")
plot_correlations(class_ratio, 2, cr_vars, val_cols) + xlab("Class ratio") +
  ggtitle("AUC") 

plot_correlations(signal_strength, 3, ss_vars, val_cols) + xlab("Signal strength") +
  ggtitle("Calibration slope")
plot_correlations(training_size, 3, ts_vars, val_cols, no_scale = T) +
  xlab("Training sample size") + ggtitle("Calibration slope")
plot_correlations(class_ratio, 3, cr_vars, val_cols) + xlab("Class ratio") +
  ggtitle("Calibration slope") 

plot_correlations(signal_strength, 4, ss_vars, val_cols) + xlab("Signal strength") +
  ggtitle("Calibration intercept")
plot_correlations(training_size, 4, ts_vars, val_cols, no_scale = T) +
  xlab("Training sample size") + ggtitle("Calibration intercept")
plot_correlations(class_ratio, 4, cr_vars, val_cols) +
  xlab("Class ratio") + ggtitle("Calibration intercept") 



y_t <- theme(legend.position = "none",
             plot.title = element_text(hjust = 0.5, size = 14),
             axis.title.y=element_text(size=14))

ac_lim <- coord_cartesian(ylim = c(0,0.35))
au_lim <- coord_cartesian(ylim = c(-0.15,0.35))
cs_lim <- coord_cartesian(ylim = c(-0.65, 0.1))
ci_lim <- coord_cartesian(ylim = c(-0.85, 0.25))

p_ac1 <- plot_correlations(signal_strength, 1, ss_vars,  val_cols, val_labels, c(1,3,5,6,7))+
  y_t + xlab("") + ggtitle("4.A") + ylab("\u03c1 (accuracy)") + ac_lim
p_ac2 <- plot_correlations(training_size, 1, ts_vars, val_cols, val_labels, c(1,3,5,6,7),no_scale=T)+
  y_t + ggtitle("4.B")+ xlab("") + ylab("") + ac_lim
p_ac3 <- plot_correlations(class_ratio, 1, cr_vars, val_cols, val_labels, c(1,3,5,6,7))+
  y_t + ggtitle("4.C")+ xlab("") + ylab("") + ac_lim
p_au1 <- plot_correlations(signal_strength, 2, ss_vars, val_cols, val_labels) +
  y_t + xlab("") + ylab("\u03c1 (AUC)") + au_lim
p_au2 <- plot_correlations(training_size, 2, ts_vars, val_cols, no_scale =  T) +
  y_t + xlab("") + ylab("") + au_lim
p_au3 <- plot_correlations(class_ratio, 2, cr_vars, val_cols) +
  y_t + xlab("") + ylab("") + au_lim
p_cs1 <- plot_correlations(signal_strength, 3, ss_vars, val_cols) +
  y_t + xlab("") + ylab("\u03c1 (CS)")  + cs_lim
p_cs2 <- plot_correlations(training_size, 3, ts_vars, val_cols, no_scale = T)+
  y_t + xlab("") + ylab("") + cs_lim
p_cs3 <- plot_correlations(class_ratio, 3, cr_vars, val_cols) +
  y_t + xlab("") + ylab("") + cs_lim
p_ci1 <- plot_correlations(signal_strength, 4, ss_vars, val_cols) +
  y_t + xlab("Signal strength") + ylab("\u03c1 (CI)") + ci_lim
p_ci2 <- plot_correlations(training_size, 4, ts_vars, val_cols, no_scale = T) +
  y_t + xlab("Training sample size") + ylab("") + ci_lim
p_ci3 <- plot_correlations(class_ratio, 4, cr_vars, val_cols) +
  y_t + xlab("Class ratio") + ylab("") + ci_lim

pg <- plot_grid(p_ac1, p_ac2, p_ac3, p_au1, p_au2, p_au3, p_cs1, p_cs2, p_cs3,
                p_ci1, p_ci2, p_ci3, nrow = 4, ncol = 3, align = "vh", labels = "AUTO")
#pg <- plot_grid(plotlist = align_plots(p_ac1, p_ac2, p_ac3, p_au1, p_au2, p_au3, p_cs1, p_cs2, p_cs3,
#                p_ci1, p_ci2, p_ci3), nrow = 4, ncol = 3, align = "vh", labels = "AUTO")
legend_b <- get_legend( p_au1 + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "bottom"))

png('../../Figures/correl.png', width = 22, height = 24, units = 'cm', res = 400)
 plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .03))
dev.off()


plot_boxplot(signal_strength$signal_strength_5, 1) +
  ggtitle("Accuracy with default setup") + xlab("") + ylab("") +
  annotate("text", 8.2, .75, label = "EP", color = "red")
plot_boxplot(signal_strength$signal_strength_5, 1, T) +
  ggtitle("Accuracy with default setup") + xlab("") + ylab("") +
  annotate("text", 7.2, .05, label = "EP[T]", color = "red", parse = T)

plot_boxplot(signal_strength$signal_strength_3, 1)
plot_boxplot(class_ratio$class_ratio_0.1, 3, val_tech = c(2,4,5,6,7)) + ylim(c(-2, 2))

#ss_legend <- as.expression(paste(seq(0,1, length.out = 6),"beta[d]",  sep = "%*%"))

ss_legend <- expression(0.0%*%beta[d],0.2%*%beta[d],0.4%*%beta[d],
                        0.6%*%beta[d],0.8%*%beta[d],1.0%*%beta[d])
ss_title <- expression(beta)

ts_names <- as.character(c(20, 40, 60, 80, 100, 200, 500, 1000))
ts_title <- expression(n[t])

cr_names <- as.character(c(0.1, 0.2, 0.3, 0.4, 0.5))

acc_tech <- c(1,3,5,6,7)


plot_grouped_boxplot2(signal_strength, 1)

p1_acc <- plot_grouped_boxplot(signal_strength, 1, red, val_tech = acc_tech,
                legend_labs = ss_legend, legend_title = ss_title) + ggtitle("Accuracy")
p2_acc <- plot_grouped_boxplot(training_size, 1, red, ts_names,
                val_tech = acc_tech, legend_title = ts_title)
p3_acc <- plot_grouped_boxplot(class_ratio, 1, red, cr_names, val_tech = acc_tech,
                legend_title = "CR")
png('../../Figures/grouped_4_acc.png', width = 16, height = 25, units = 'cm', res = 400)
plot_grid(p1_acc, p2_acc, p3_acc, align = "v", nrow = 3, labels = "AUTO")
dev.off()


p1_auc <- plot_grouped_boxplot(signal_strength, 2, orange, legend_labs = ss_legend,
                               legend_title = ss_title) + ggtitle("AUC")
p2_auc <- plot_grouped_boxplot(training_size, 2, orange, ts_names, legend_title = ts_title)
p3_auc <- plot_grouped_boxplot(class_ratio, 2, orange, cr_names, legend_title = "CR")

png('../../Figures/grouped_4_AUC.png', width = 16, height = 25, units = 'cm', res = 400)
plot_grid(p1_auc, p2_auc, p3_auc, align = "v", nrow = 3, labels = "AUTO")
dev.off()

p1_cs <- plot_grouped_boxplot(signal_strength, 3, blue, legend_labs = ss_legend,
  legend_title = ss_title) + ggtitle("Calibration slope") + coord_cartesian(ylim = c(-1, 1))
p2_cs <- plot_grouped_boxplot(training_size, 3, blue, ts_names, legend_title = ts_title) +
  coord_cartesian(ylim = c(-1, 1))
p3_cs <- plot_grouped_boxplot(class_ratio, 3, blue, cr_names, legend_title = "CR") +
  coord_cartesian(ylim = c(-1, 1))

png('../../Figures/grouped_4_CS.png', width = 16, height = 25, units = 'cm', res = 400)
plot_grid(p1_cs, p2_cs, p3_cs, align = "v", nrow = 3, labels = "AUTO")
dev.off()


p1_ci <- plot_grouped_boxplot(signal_strength, 4, purple, legend_labs = ss_legend,
  legend_title = ss_title) + ggtitle("Calibration intercept") + coord_cartesian(ylim = c(-1, 1))
p2_ci <- plot_grouped_boxplot(training_size, 4, purple, ts_names, legend_title = ts_title) +
  coord_cartesian(ylim = c(-2, 2))
p3_ci<- plot_grouped_boxplot(class_ratio, 4, purple, cr_names, legend_title = "CR") +
  coord_cartesian(ylim = c(-2, 2))

png('../../Figures/grouped_4_CI.png', width = 16, height = 25, units = 'cm', res = 400)
plot_grid(p1_ci, p2_ci, p3_ci, align = "v", nrow = 3,  labels = "AUTO")
dev.off()

plot_grouped_boxplot(signal_strength, 4, purple, "", NULL, F, legend_labs = ss_legend) +
  ggtitle("Calibration intercept") + ylim(c(-1, 1))
plot_grouped_boxplot(training_size, 4, purple, "", ts_names, T, legend_title = ts_title) +
  ylim(c(-2, 2))
plot_grouped_boxplot(class_ratio, 4, purple, "", cr_names, T, legend_title = "CR") +
  ylim(c(-2, 2))




pm_cols <- c(red, orange, blue, purple)

calculate_EP(training_size)
plot_EP(signal_strength, ss_vars, col = pm_cols)
plot_EP(training_size, ts_vars, col = pm_cols, no_scale = T)
plot_EP(class_ratio, cr_vars, pm_cols)


no_legend <-  theme(legend.position = "none", 
                   plot.title = element_text(hjust = 0.5))

p1 <- plot_EP(signal_strength, ss_vars, pm_cols) +
  ggtitle("4.A") + xlab("Signal strength") + ylab("EP") + no_legend
p2 <- plot_EP(training_size, ts_vars, pm_cols, no_scale = T) +
  ggtitle("4.B") + xlab("Training sample size") + ylab("") + no_legend
p3 <- plot_EP(class_ratio, cr_vars, pm_cols) +
  ggtitle("4.C") + xlab("Class ratio") + ylab ("") + no_legend
 

pg <- plot_grid(plotlist = align_plots(p1, p2, p3), nrow = 1, ncol = 3,
                labels = "AUTO")
legend_b <- get_legend( p1 + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "bottom"))

png('../../Figures/EP_4.png', width = 22, height = 8, units = 'cm', res = 400)
plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .1))
dev.off()

plot_comparison(signal_strength, 1, ss_vars, F, val_cols, val_labels, c(1,3,5,6,7)) 
plot_comparison(training_size, 1, ts_vars, F, val_cols, val_labels, c(1,3,5,6,7), no_scale = T)
plot_comparison(class_ratio, 1, cr_vars, F, val_cols, val_labels, c(1,3,5,6,7))

plot_comparison(signal_strength, 1, ss_vars, T, val_cols, val_labels, c(1,3,5,6,7))
plot_comparison(training_size, 1, ts_vars, T, val_cols, val_labels, c(1,3,5,6,7), no_scale = T)
plot_comparison(class_ratio, 1, cr_vars, T, val_cols, val_labels, c(1,3,5,6,7))

plot_comparison(signal_strength, 2, ss_vars, F, val_cols)
plot_comparison(training_size, 2, ts_vars, F, val_cols, no_scale = T)
plot_comparison(class_ratio, 2, cr_vars, F, val_cols)

plot_comparison(signal_strength, 2, ss_vars, T, val_cols)
plot_comparison(training_size, 2, ts_vars, T, val_cols, no_scale = T)
plot_comparison(class_ratio, 2, cr_vars, T, val_cols)


plot_comparison(signal_strength, 3, ss_vars, F, val_cols) +
  coord_cartesian(ylim = c(-.5, .5))
plot_comparison(training_size, 3, ts_vars, F, val_cols, no_scale = T) +
  coord_cartesian(ylim = c(-.5, .5))
plot_comparison(class_ratio, 3, cr_vars, F, val_cols) +
  coord_cartesian(ylim = c(-.5, .5))

plot_comparison(signal_strength, 3, ss_vars, T, val_cols) +
  coord_cartesian(ylim = c(0, .5))
plot_comparison(training_size, 3, ts_vars, T, val_cols, no_scale = T)  +
  coord_cartesian(ylim = c(0, .5))
plot_comparison(class_ratio, 3, cr_vars, T, val_cols) +
  coord_cartesian(ylim = c(0, .5))


plot_comparison(signal_strength, 4, ss_vars, F, val_cols) +
  coord_cartesian(ylim = c(-.5, .5))
plot_comparison(training_size, 4, ts_vars, F, val_cols, no_scale = T) +
  coord_cartesian(ylim = c(-.5, .5))
plot_comparison(class_ratio, 4, cr_vars, F, val_cols) +
  coord_cartesian(ylim = c(-1, 1))

plot_comparison(signal_strength, 4, ss_vars, T, val_cols) +
  coord_cartesian(ylim = c(0, .5))
plot_comparison(training_size, 4, ts_vars, T, val_cols, no_scale = T)  +
  coord_cartesian(ylim = c(0, .75))
plot_comparison(class_ratio, 4, cr_vars, T, val_cols) +
  coord_cartesian(ylim = c(0, .5))

no_legend <-  theme(legend.position = "none", 
                    plot.title = element_text(hjust = 0.5))
xl_ss <- xlab("Signal strength")
xl_ts <- xlab("Training sample size")
xl_cr <- xlab("Class ratio")

p1 <- plot_comparison(signal_strength, 1, ss_vars, F, val_cols, val_labels, c(1,3,5,6,7)) + no_legend + xlab("") + ggtitle("4.A")
p2 <- plot_comparison(training_size, 1, ts_vars, F, val_cols, val_labels, c(1,3,5,6,7), no_scale = T)+no_legend+xlab("")+ggtitle("4.B")
p3 <- plot_comparison(class_ratio, 1, cr_vars, F, val_cols, val_labels, c(1,3,5,6,7)) + no_legend  + xlab("") + ggtitle("4.C")
p4 <- plot_comparison(signal_strength, 1, ss_vars, T, val_cols, val_labels, c(1,3,5,6,7)) + no_legend + xl_ss
p5 <- plot_comparison(training_size, 1, ts_vars, T, val_cols, val_labels, c(1,3,5,6,7), no_scale = T) + no_legend + xl_ts
p6 <- plot_comparison(class_ratio, 1, cr_vars, T, val_cols, val_labels, c(1,3,5,6,7)) + no_legend + xl_cr

pg <- plot_grid(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, align = "vh",
                labels = "AUTO", rel_heights = c(1, .9))
# pg <- plot_grid(plotlist = c(align_plots(p1, p2, p3), align_plots(p4, p5, p6)),
#                nrow = 2, ncol = 3, align = "vh", labels = "AUTO",
#                rel_heights = c(1, .9))
legend_b <- get_legend( p1 + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "bottom"))

png('../../Figures/acc_bias_rmse.png', width = 22, height = 15, units = 'cm', res = 400)
plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .05))
dev.off()

p1 <- plot_comparison(signal_strength, 2, ss_vars, F, val_cols, val_labels) + no_legend + xlab("") + ggtitle("4.A")
p2 <- plot_comparison(training_size, 2, ts_vars, F, val_cols, no_scale = T) + no_legend + xlab("") + ggtitle("4.B")
p3 <- plot_comparison(class_ratio, 2, cr_vars, F, val_cols) + no_legend + xlab("") + ggtitle("4.C")
p4 <- plot_comparison(signal_strength, 2, ss_vars, T, val_cols) + no_legend + xl_ss
p5 <- plot_comparison(training_size, 2, ts_vars, T, val_cols, no_scale = T) + no_legend + xl_ts
p6 <- plot_comparison(class_ratio, 2, cr_vars, T, val_cols) + no_legend + xl_cr

pg <- plot_grid(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, align = "vh",
                labels = "AUTO", rel_heights = c(1, .9))
legend_b <- get_legend( p1 + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "bottom"))

png('../../Figures/auc_bias_rmse.png', width = 22, height = 15, units = 'cm', res = 400)
plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .05))
dev.off()

p1 <- plot_comparison(signal_strength, 3, ss_vars, F, val_cols, val_labels) +
  coord_cartesian(ylim = c(-.5, .5)) + no_legend + xlab("") + ggtitle("4.A")
p2 <- plot_comparison(training_size, 3, ts_vars, F, val_cols, no_scale = T) +
  coord_cartesian(ylim = c(-.5, .5)) + no_legend + xlab("") + ggtitle("4.B")
p3 <- plot_comparison(class_ratio, 3, cr_vars, F, val_cols) +
  coord_cartesian(ylim = c(-.5, .5)) + no_legend + xlab("") + ggtitle("4.C")
p4 <- plot_comparison(signal_strength, 3, ss_vars, T, val_cols) +
  coord_cartesian(ylim = c(0, .5)) + no_legend + xl_ss
p5 <- plot_comparison(training_size, 3, ts_vars, T, val_cols, no_scale = T)  +
  coord_cartesian(ylim = c(0, .5)) + no_legend + xl_ts
p6 <- plot_comparison(class_ratio, 3, cr_vars, T, val_cols) +
  coord_cartesian(ylim = c(0, .5)) + no_legend + xl_cr

pg <- plot_grid(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, align = "vh",
                labels = "AUTO", rel_heights = c(1, .9))
legend_b <- get_legend( p1 + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "bottom"))

png('../../Figures/cs_bias_rmse.png', width = 22, height = 15, units = 'cm', res = 400)
plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .05))
dev.off()


p1 <- plot_comparison(signal_strength, 4, ss_vars, F, val_cols, val_labels) +
  coord_cartesian(ylim = c(-.5, .5)) + no_legend + xlab("") + ggtitle("4.A")
p2 <- plot_comparison(training_size, 4, ts_vars, F, val_cols, no_scale = T) +
  coord_cartesian(ylim = c(-.5, .5)) + no_legend + xlab("") + ggtitle("4.B")
p3 <- plot_comparison(class_ratio, 4, cr_vars, F, val_cols) +
  coord_cartesian(ylim = c(-1, 1)) + no_legend + xlab("") + ggtitle("4.C")
p4 <- plot_comparison(signal_strength, 4, ss_vars, T, val_cols) +
  coord_cartesian(ylim = c(0, .5)) + no_legend + xl_ss
p5 <- plot_comparison(training_size, 4, ts_vars, T, val_cols, no_scale = T)  +
  coord_cartesian(ylim = c(0, .75)) + no_legend + xl_ts
p6 <- plot_comparison(class_ratio, 4, cr_vars, T, val_cols) +
  coord_cartesian(ylim = c(0, .5)) + no_legend + xl_cr

pg <- plot_grid(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, align = "vh",
                labels = "AUTO", rel_heights = c(1, .9))
legend_b <- get_legend( p1 + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "bottom"))

png('../../Figures/ci_bias_rmse.png', width = 22, height = 15, units = 'cm', res = 400)
plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .05))
dev.off()

#~~~~~~~~~~~~~~~~~~~

# perf_meas <- 2
# results <- signal_strength
# 
# val_tech <- NULL
# 
# long_df <- data.frame(values = numeric(0), ind = character(0), sim_type = character(0))
# 
# for (i in 1:length(results)){
#   result_array <- results[[i]]
#   len_val_tech <- dim(result_array)[1]
#   if (is.null(val_tech)) val_tech = 1:(len_val_tech-1)
#   result_df <- as.data.frame(t(results[[i]][c(val_tech, len_val_tech), perf_meas,]))
#   col_conditional <- ncol(result_df)
#   
#   result_df <- result_df - mean(result_df[,col_conditional], na.rm = TRUE)
#   
#   long_format <- stack(result_df)
#   long_format$sim_type <- var_names[i]
#   long_df <- rbind(long_df, long_format)
# }
# 
# alpha_values <- as.hexmode(2:7*30)
# 
# #lbl <- "beta == list(0, 1, -1)"
# #annotate("text", x = Inf, y = .1, label = lbl, parse = TRUE) + 
# 
# ggplot(long_df, aes(x = ind, y = values, fill = sim_type)) +
#   geom_boxplot(color = "gray34", outlier.alpha = .1, outlier.colour = orange) +
#   scale_fill_manual(values = paste(orange, alpha_values, sep = "")) + 
#   theme(legend.justification = "bottom")
  