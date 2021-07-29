library(ggplot2)
library(cowplot)
library(xtable)


setwd("..")
source("evaluation/evaluating_results.R")
source("data_generation.R")
source("performance_measures.R")

load("data/penalized_A.RData")
A_penalized <- all_results
load("data/penalized_B.RData")
B_penalized <- all_results

load("data/penalized_A_uncor.RData")
A_penalized <- all_results
load("data/penalized_B_uncor.RData")
B_penalized <- all_results

p_vars <- c(10,20,50,100,200)
p_title <- "p"

n_sim <- dim(A_penalized$n_pred_10)[[3]]

# % not better than null model:
sapply(A_penalized, \(x) sum(x[8,1,] < .51)) / n_sim * 100
sapply(A_penalized, \(x) sum(x[8,6,] < .501)) / n_sim * 100

sapply(B_penalized, \(x) sum(x[8,1,] < .501)) / n_sim * 100
sapply(B_penalized, \(x) sum(x[8,6,] < .51)) / n_sim * 100

sapply(A_penalized, \(x) sum(x[8,7,] < .51)) / n_sim * 100
sapply(B_penalized, \(x) sum(x[8,2,] < .501)) / n_sim * 100

sapply(A_penalized, \(x) sum(is.na(x[8,8,]))) / n_sim * 100
sapply(B_penalized, \(x) sum(is.na(x[8,3,]))) / n_sim * 100



plot_correlations(A_penalized, 1, p_vars, val_cols, no_scale = T) 
plot_correlations(A_penalized, 6, p_vars, val_cols, no_scale = T)
plot_correlations(A_penalized, 2, p_vars, val_cols, no_scale = T)
plot_correlations(A_penalized, 7, p_vars, val_cols, no_scale = T)
plot_correlations(A_penalized, 3, p_vars, val_cols, no_scale = T)
plot_correlations(A_penalized, 8, p_vars, val_cols, no_scale = T)
plot_estimates_and_EP(A_penalized$n_pred_50, 1, 5)
plot_estimates_and_EP(A_penalized$n_pred_200, 6, 1)

plot_correlations(B_penalized, 1, p_vars, val_cols, no_scale = T)
plot_correlations(B_penalized, 6, p_vars, val_cols, no_scale = T)
plot_correlations(B_penalized, 2, p_vars, val_cols, no_scale = T)
plot_correlations(B_penalized, 7, p_vars, val_cols, no_scale = T)
plot_correlations(B_penalized, 3, p_vars, val_cols, no_scale = T)
plot_correlations(B_penalized, 8, p_vars, val_cols, no_scale = T)


plot_estimates_and_EP(A_penalized$n_pred_200, 6, 1)
plot_estimates_and_EP(B_penalized$n_pred_200, 1, 5)

plot_estimates_and_EP(B_penalized$n_pred_10, 4, 2)
plot_estimates_and_EP(A_penalized$n_pred_10, 8, 4)

no_l <-  theme(legend.position = "none",
               plot.title = element_text(hjust = 0.5, size = 14),
               legend.title.align=0.5,
               axis.title.y=element_text(size=14))
y_scale <- scale_y_continuous(breaks=c(-1, -.5, 0, .5, 1), limits=c(-1, 1))

plot_estimates_and_EP(B_penalized$n_pred_200, 1, 5)
plot_estimates_and_EP(B_penalized$n_pred_10, 4, 6)


p1 <- plot_correlations(A_penalized, 6, p_vars, val_cols, val_labels, 
  c(1,3,5,6,7), no_scale = T) + no_l + ggtitle("Simulation 5.A (lasso)") +
  ylab("\u03c1 (accuracy)") + xlab("") + y_scale
p2 <- plot_correlations(B_penalized, 1, p_vars, val_cols, val_labels, 
  c(1,3,5,6,7), no_scale = T) + no_l + ggtitle("Simulation 5.B (ridge)") +
  ylab("") + xlab("") + y_scale
p3 <- plot_correlations(A_penalized, 7, p_vars, val_cols, val_labels, 
  no_scale = T) + no_l + ylab("\u03c1 (AUC)") + xlab("") + y_scale
p4 <- plot_correlations(B_penalized, 2, p_vars, val_cols, val_labels, 
  no_scale = T) + no_l + ylab("") + xlab("") + y_scale
p5 <- plot_correlations(A_penalized, 8, p_vars, val_cols, val_labels, 
  no_scale = T) + no_l + ylab("\u03c1 (CS)") + xlab("") + y_scale
p6 <- plot_correlations(B_penalized, 3, p_vars, val_cols, val_labels, 
  no_scale = T) + no_l + ylab("") + xlab("") + y_scale
p7 <- plot_correlations(A_penalized, 9, p_vars, val_cols, val_labels, 
  no_scale = T) + no_l + ylab("\u03c1 (CI)") + xlab("p") + y_scale
p8 <- plot_correlations(B_penalized, 4, p_vars, val_cols, val_labels, 
  no_scale = T) + no_l + ylab("") + xlab("p") + y_scale

#pg <-  plot_grid(plotlist = align_plots(p1, p2, p3, p4, p5, p6, p7, p8))
pg <-  plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 4, align = "vh")
legend_b <- get_legend(p3 + guides(color = guide_legend(nrow = 1)) + 
                            theme(legend.position = "bottom"))

png('../Figures/penal_correl.png', width = 16, height = 24, units = 'cm', res = 400)
plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .03))
dev.off()

PM_cols <- c(red, orange, blue, purple)

xtable(calculate_EP(A_penalized)[6:9,])
xtable(calculate_EP(B_penalized)[1:4,])

plot_EP(A_penalized, p_vars, PM_cols, 1:4, no_scale = T)
plot_EP(A_penalized, p_vars, PM_cols, 6:9, no_scale = T)

plot_EP(B_penalized, p_vars, PM_cols, 1:4, no_scale = T)
plot_EP(B_penalized, p_vars, PM_cols, 6:9, no_scale = T)


# Accuracy -------------------------------------------------------
cond <- FALSE #set whether EP or EP_T should be compared

no_l <-  theme(legend.position = "none", 
               legend.title.align=0.5,
               plot.title = element_text(hjust = 0.5))
box_ylab <- ylab(expression(P[V]-EP))

p1 <- plot_grouped_boxplot(A_penalized, 6, red, NULL, c(1,3,5,6,7), p_vars, "p",
  conditional = cond) + no_l + ggtitle("5.A (lasso)") + box_ylab
  
p2 <- plot_grouped_boxplot(B_penalized, 1, red, NULL, c(1,3,5,6,7), p_vars, "p",
                           conditional = cond) + no_l + ggtitle("5.B (ridge)")

p3 <- plot_comparison(A_penalized, 6, p_vars, F, val_cols, val_labels,
                      c(1,3,5,6,7), cond, T, "VT") + no_l + xlab("")
p4 <- plot_comparison(B_penalized, 1, p_vars, F, val_cols, val_labels,
                      c(1,3,5,6,7), cond, T, "VT") + no_l + xlab("")

p5 <- plot_comparison(A_penalized, 6, p_vars, T, val_cols, val_labels,
                      c(1,3,5,6,7), cond, T, "VT") + no_l + xlab("p")
p6 <- plot_comparison(B_penalized, 1, p_vars, T, val_cols, val_labels,
                      c(1,3,5,6,7), cond, T, "VT") + no_l + xlab("p")

# pg1 <- plot_grid(p1, p2, nrow = 1, labels = "AUTO")
# legend_b1 <- get_legend(p1  + guides(color = guide_legend(nrow = 1)) + 
#                           theme(legend.position = "bottom"))
# pg1 <- plot_grid(pg1, legend_b1, ncol = 1, rel_heights = c(1, .07))
# 
# pg2 <- plot_grid(p3, p4, p5, p6, nrow = 2, align = "vh", labels = LETTERS[3:6])
# legend_b2 <- get_legend( p3 + guides(color = guide_legend(nrow = 1)) + 
#                           theme(legend.position = "bottom"))
# pg2 <- plot_grid(pg2, legend_b2, ncol = 1, rel_heights = c(1, .07))
# pg <- plot_grid(pg1, pg2, nrow = 2, rel_heights = c(1, 2))
# 
# png('../Figures/penalized_acc.png', width = 18, height = 24, units = 'cm', res = 400)
# pg
# dev.off()


pg1 <- plot_grid(p1, p2, nrow = 1, labels = "AUTO")
legend_b1 <- get_legend(p1 + theme(legend.position = "right"))
pg1 <- plot_grid(pg1, legend_b1, ncol = 2, rel_widths = c(1, .12))

pg2 <- plot_grid(p3, p4, p5, p6, nrow = 2, align = "vh", labels = LETTERS[3:6])
legend_b2 <- get_legend(p3 + theme(legend.position = "right"))
pg2 <- plot_grid(pg2, legend_b2, ncol = 2, rel_widths = c(1, .12))
pg <- plot_grid(pg1, pg2, nrow = 2, rel_heights = c(1, 2))

png('../Figures/penalized_acc.png', width = 18, height = 24, units = 'cm', res = 400)
pg
dev.off()



# AUC ---------------------------------------------------------------------

p1 <- plot_grouped_boxplot(A_penalized, 7, orange, NULL, NULL, p_vars, "p", cond) +
  no_l + ggtitle("5.A (lasso)") + box_ylab
p2 <- plot_grouped_boxplot(B_penalized, 2, orange, NULL, NULL, p_vars, "p", cond) +
  no_l + ggtitle("5.B (ridge)")

p3 <- plot_comparison(A_penalized, 7, p_vars, F, val_cols, val_labels, NULL,
                      cond, T, "VT") + no_l + xlab("")
p4 <- plot_comparison(B_penalized, 2, p_vars, F, val_cols, val_labels, NULL,
                      cond, T, "VT") + no_l + xlab("")
p5 <- plot_comparison(A_penalized, 7, p_vars, T, val_cols, val_labels, NULL,
                      cond, T, "VT") + no_l + xlab("p")
p6 <- plot_comparison(B_penalized, 2, p_vars, T, val_cols, val_labels, NULL,
                      cond, T, "VT") + no_l + xlab("p")


pg1 <- plot_grid(p1, p2, nrow = 1, labels = "AUTO")
legend_b1 <- get_legend(p1  + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "right"))
pg1 <- plot_grid(pg1, legend_b1, ncol = 2, rel_widths = c(1, .12))

pg2 <- plot_grid(p3, p4, p5, p6, nrow = 2, align = "vh", labels = LETTERS[3:6])
legend_b2 <- get_legend(p3 + theme(legend.position = "right"))
pg2 <- plot_grid(pg2, legend_b2, ncol = 2, rel_widths = c(1, .12))
pg <- plot_grid(pg1, pg2, nrow = 2, rel_heights = c(1, 2))

png('../Figures/penalized_auc.png', width = 18, height = 24, units = 'cm', res = 400)
pg
dev.off()


# Calibration slope -------------------------------------------------------
p1 <- plot_grouped_boxplot(A_penalized, 8, blue, NULL, NULL, p_vars, "p", cond) +
  no_l + ggtitle("5.A (lasso)") + coord_cartesian(ylim = c(-5, 5)) + box_ylab
p2 <- plot_grouped_boxplot(B_penalized, 3, blue, NULL, NULL, p_vars, "p", cond) +
  no_l + ggtitle("5.B (ridge)") + coord_cartesian(ylim = c(-5, 5))

p3 <- plot_comparison(A_penalized, 8, p_vars, F, val_cols, val_labels, NULL,
                      cond, T, "VT") + no_l + xlab("")
p4 <- plot_comparison(B_penalized, 3, p_vars, F, val_cols, val_labels, NULL,
     cond, T, "VT") + no_l + xlab("")  + coord_cartesian(ylim = c(-5, 5))
p5 <- plot_comparison(A_penalized, 8, p_vars, T, val_cols, val_labels, NULL,
                      cond, T, "VT") + no_l + xlab("")
p6 <- plot_comparison(B_penalized, 3, p_vars, T, val_cols, val_labels, NULL,
     cond, T, "VT") + no_l + xlab("") + coord_cartesian(ylim = c(0,30))


pg1 <- plot_grid(p1, p2, nrow = 1, labels = "AUTO")
legend_b1 <- get_legend(p1  + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "right"))
pg1 <- plot_grid(pg1, legend_b1, ncol = 2, rel_widths = c(1, .12))

pg2 <- plot_grid(p3, p4, p5, p6, nrow = 2, align = "vh", labels = LETTERS[3:6])
legend_b2 <- get_legend(p3 + theme(legend.position = "right"))
pg2 <- plot_grid(pg2, legend_b2, ncol = 2, rel_widths = c(1, .12))
pg <- plot_grid(pg1, pg2, nrow = 2, rel_heights = c(1, 2))

png('../Figures/penalized_cs.png', width = 18, height = 24, units = 'cm', res = 400)
pg
dev.off()


# CI ----------------------------------------------------------------------

p1 <- plot_grouped_boxplot(A_penalized, 9, purple, NULL, NULL, p_vars, "p", cond) +
  no_l + ggtitle("5.A (lasso)") + coord_cartesian(ylim = c(-3, 3)) + box_ylab
p2 <- plot_grouped_boxplot(B_penalized, 4, purple, NULL, NULL, p_vars, "p", cond) +
  no_l + ggtitle("5.B (ridge)") + coord_cartesian(ylim = c(-3, 3))

p3 <- plot_comparison(A_penalized, 9, p_vars, F, val_cols, val_labels, NULL,
                      cond, T, "VT") + no_l + xlab("")
p4 <- plot_comparison(B_penalized, 4, p_vars, F, val_cols, val_labels, NULL,
                      cond, T, "VT") + no_l + xlab("")
p5 <- plot_comparison(A_penalized, 9, p_vars, T, val_cols, val_labels, NULL,
                      cond, T, "VT") + no_l + xlab("")
p6 <- plot_comparison(B_penalized, 4, p_vars, T, val_cols, val_labels, NULL,
                      cond, T, "VT") + no_l + xlab("") 


pg1 <- plot_grid(p1, p2, nrow = 1, labels = "AUTO")
legend_b1 <- get_legend(p1  + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "right"))
pg1 <- plot_grid(pg1, legend_b1, ncol = 2, rel_widths = c(1, .12))

pg2 <- plot_grid(p3, p4, p5, p6, nrow = 2, align = "vh", labels = LETTERS[3:6])
legend_b2 <- get_legend(p3 + theme(legend.position = "right"))
pg2 <- plot_grid(pg2, legend_b2, ncol = 2, rel_widths = c(1, .12))
pg <- plot_grid(pg1, pg2, nrow = 2, rel_heights = c(1, 2))

png('../Figures/penalized_ci.png', width = 18, height = 24, units = 'cm', res = 400)
pg
dev.off()


# Old stuff ---------------------------------------------------------------

p1 <- plot_grouped_boxplot(A_penalized, 1, red, legend_labs = p_vars, legend_title = "p") + no_l 
p2 <- plot_grouped_boxplot(A_penalized, 6, red, legend_labs = p_vars, legend_title = "p", conditional = T) + no_l
p3 <- plot_grouped_boxplot(B_penalized, 1, red, legend_labs = p_vars, legend_title = "p", conditional = T) + no_l
p4 <- plot_grouped_boxplot(B_penalized, 6, red, legend_labs = p_vars, legend_title = "p") + no_l

pg <- plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2, align = "vh", labels = "AUTO")
legend_b <- get_legend( p1 + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "bottom"))

png('../Figures/penalized_acc.png', width = 22, height = 15, units = 'cm', res = 400)
plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .05))
dev.off()

#Accuracy
p1 <- plot_comparison(A_penalized, 1, p_vars, F, val_cols, val_labels, no_scale = T) + no_l + ggtitle("5.A, ridge") + xlab("")
p2 <- plot_comparison(A_penalized, 6, p_vars, F, val_cols, val_labels, no_scale = T) + no_l + ggtitle("5.A, lasso") + xlab("")
p3 <- plot_comparison(B_penalized, 1, p_vars, F, val_cols, val_labels, no_scale = T) + no_l + ggtitle("5.B, ridge") + xlab("")
p4 <- plot_comparison(B_penalized, 6, p_vars, F, val_cols, val_labels, no_scale = T) + no_l + ggtitle("5.B, lasso") + xlab("")

p5 <- plot_comparison(A_penalized, 1, p_vars, T, val_cols, val_labels, no_scale = T) + no_l + xlab("")
p6 <- plot_comparison(A_penalized, 6, p_vars, T, val_cols, val_labels, no_scale = T) + no_l + xlab("")
p7 <- plot_comparison(B_penalized, 1, p_vars, T, val_cols, val_labels, no_scale = T) + no_l + xlab("")
p8 <- plot_comparison(B_penalized, 6, p_vars, T, val_cols, val_labels, no_scale = T) + no_l + xlab("")

pg <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8,
                nrow = 2, ncol = 4, align = "vh", labels = "AUTO")
legend_b <- get_legend( p1 + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "bottom"))

png('../Figures/penalized_acc_comp.png', width = 22, height = 15, units = 'cm', res = 400)
plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .05))
dev.off()

#AUC
plot_comparison(A_penalized, 2, p_vars, F, val_cols, val_labels, no_scale = T)
plot_comparison(A_penalized, 7, p_vars, F, val_cols, val_labels, no_scale = T)
plot_comparison(A_penalized, 2, p_vars, T, val_cols, val_labels, no_scale = T)
plot_comparison(A_penalized, 7, p_vars, T, val_cols, val_labels, no_scale = T)

plot_comparison(B_penalized, 2, p_vars, F, val_cols, val_labels, no_scale = T)
plot_comparison(B_penalized, 7, p_vars, F, val_cols, val_labels, no_scale = T)
plot_comparison(B_penalized, 2, p_vars, T, val_cols, val_labels, no_scale = T)
plot_comparison(B_penalized, 7, p_vars, T, val_cols, val_labels, no_scale = T)

p1 <- plot_comparison(A_penalized, 2, p_vars, F, val_cols, val_labels, no_scale = T) + no_l + ggtitle("5.A, ridge") + xlab("")
p2 <- plot_comparison(A_penalized, 7, p_vars, F, val_cols, val_labels, no_scale = T) + no_l + ggtitle("5.A, lasso") + xlab("")
p3 <- plot_comparison(B_penalized, 2, p_vars, F, val_cols, val_labels, no_scale = T) + no_l + ggtitle("5.B, ridge") + xlab("")
p4 <- plot_comparison(B_penalized, 7, p_vars, F, val_cols, val_labels, no_scale = T) + no_l + ggtitle("5.B, lasso") + xlab("")

p5 <- plot_comparison(A_penalized, 2, p_vars, T, val_cols, val_labels, no_scale = T) + no_l + xlab("")
p6 <- plot_comparison(A_penalized, 7, p_vars, T, val_cols, val_labels, no_scale = T) + no_l + xlab("")
p7 <- plot_comparison(B_penalized, 2, p_vars, T, val_cols, val_labels, no_scale = T) + no_l + xlab("")
p8 <- plot_comparison(B_penalized, 7, p_vars, T, val_cols, val_labels, no_scale = T) + no_l + xlab("")

pg <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8,
                nrow = 2, ncol = 4, align = "vh", labels = "AUTO")
legend_b <- get_legend( p1 + guides(color = guide_legend(nrow = 1)) + 
                          theme(legend.position = "bottom"))

png('../Figures/penalized_auc_comp.png', width = 22, height = 15, units = 'cm', res = 400)
plot_grid(pg, legend_b, ncol = 1, rel_heights = c(1, .05))
dev.off()



cc_b <- coord_cartesian(ylim = c(-1, 1))
cc_rm <- coord_cartesian(ylim = c(0, 3)) 
plot_comparison(A_penalized, 3, p_vars, F, val_cols, val_labels, no_scale = T) +cc
plot_comparison(A_penalized, 8, p_vars, F, val_cols, val_labels, no_scale = T) +cc
plot_comparison(A_penalized, 3, p_vars, T, val_cols, val_labels, no_scale = T) +cc_rm
plot_comparison(A_penalized, 8, p_vars, T, val_cols, val_labels, no_scale = T) +cc_rm

plot_comparison(B_penalized, 3, p_vars, F, val_cols, val_labels, no_scale = T)
plot_comparison(B_penalized, 8, p_vars, F, val_cols, val_labels, no_scale = T)
plot_comparison(B_penalized, 3, p_vars, T, val_cols, val_labels, no_scale = T) + cc_rm
plot_comparison(B_penalized, 8, p_vars, T, val_cols, val_labels, no_scale = T) + cc_rm
