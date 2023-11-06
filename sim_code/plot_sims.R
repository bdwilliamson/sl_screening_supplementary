#load packages
library("tidyverse")
library("here")
library("cowplot")
library("folderfun")

# inputPath="/Users/drewking/R_Workspace/sl_screening/sim_output"
# outputPath="/Users/drewking/R_Workspace/sl_screening/plots"
# 
# setff("In", inputPath)
# setff("Out", outputPath)

# output <- read_csv(file=ffIn("sim_out.csv"))
output <- read_csv(file = here("sim_output", "sim_out.csv"))

# #Calculate the bias of the data
# bias_init <- output$point_est - output$truth
# cover_init <- (ciu >= output$truth & output$cil <= output$truth) %>%
#   group_by(n, p, fam, link, x_dist, linear, weak, est_type) %>%
#   summarize (bias = mean(bias_init), coverage = mean(cover_init), viariance = var(point_est))

# Build a plot from lasso data
lasso_sims <- as_tibble(output) %>% 
  filter(est_type=="lasso") %>% mutate(outlier = point_est < 0)
lasso_plot <- ggplot(data=lasso_sims %>% filter(!outlier)) +
        geom_boxplot(aes(x=x_dist, y=point_est,color=linear), outlier.shape=NA)+
  scale_color_viridis_d(option = "plasma") +
        labs(x = "Distribution", y = "Prediction Performance")+
        ggtitle("Lasso Performance, normal vs non normal.") +
  facet_wrap(~ fam + link, labeller = label_both)

#Build a plot from superlearner data
superlearner_sims <- as_tibble(output)%>% 
  filter(est_type=="SL") %>% mutate(outlier = point_est < 0)
superlearner_plot <- ggplot(data=superlearner_sims %>% filter(!outlier)) +
  geom_boxplot(aes(x=x_dist, y=point_est, color=linear), outlier.shape=NA)+
  scale_color_viridis_d(option = "plasma") +
  labs(x = "Distribution", y = "")+
  ggtitle("SuperLearner Performance, normal vs non normal.") +
  facet_wrap(~ fam + link, labeller = label_both)

####Construct plot comparing Lasso and SL performance####
shared_legend <- get_legend(lasso_plot + theme(legend.direction = "horizontal"))
plot_grid(
  plot_grid(lasso_plot + theme(legend.position = "none"),
            superlearner_plot + theme(legend.position = "none")),
  shared_legend, nrow = 2, ncol = 1, rel_heights = c(1, .1)
)

ggsave(
  filename = ffOut("sim_performance.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12,
  height = 6,
  dpi = 600,
)

# note that the above plot pools performance across weak and strong relationships
# and all dimensions and sample sizes; for simplicity, let's stick with strong relationship,
# p = 500, and n = 3000 (matches with the performance metric plot for the poster)
simple_weak <- FALSE
simple_p <- 500
simple_n <- 3000
# subset to the prespecified values and create new variables (for labels)
plot_tib <- output %>% 
  filter(point_est > 0, p == simple_p, weak == simple_weak, n == simple_n) %>%
  mutate(`Estimator` = factor(case_when(
    est_type == "lasso" ~ 1,
    est_type == "SL" ~ 2,
    est_type == "SL_screen_lasso_only" ~ 3,
    est_type == "SL_screen" ~ 4,
    est_type == "SL_screen_lasso" ~ 5
  ), levels = 1:5, labels = c("Lasso", "SL",
                              "SL with lasso screen",
                              "SL with screens (-lasso)",
                              "SL with all screens")),
  n_fct = factor(n), p = factor(p),
  `Outcome relationship` = factor(linear, levels = c(TRUE, FALSE), labels = c("Linear", "Nonlinear"),
                                  ordered = TRUE),
  `X distribution` = factor(x_dist, levels = c("normal", "nonnormal"), labels = c("Normal", "Nonnormal"),
                            ordered = TRUE),
  `Strength of relationship` = factor(weak, levels = c(TRUE, FALSE), labels = c("Weak", "Strong"),
                                      ordered = TRUE),
  `Outcome type` = factor(fam, levels = c("gaussian", "binomial"), labels = c("Continuous", "Binary")))
true_perfs <- output %>% 
  filter(point_est > 0, p == simple_p, weak == simple_weak) %>% 
  group_by(fam, link, x_dist, linear, weak) %>% 
  summarize(truth = mean(truth), .groups = "drop") %>% 
  mutate(`Outcome relationship` = factor(linear, levels = c(TRUE, FALSE), labels = c("Linear", "Nonlinear"),
                                         ordered = TRUE),
         `X distribution` = factor(x_dist, levels = c("normal", "nonnormal"), labels = c("Normal", "Nonnormal"),
                                   ordered = TRUE),
         `Strength of relationship` = factor(weak, levels = c(TRUE, FALSE), labels = c("Weak", "Strong"),
                                             ordered = TRUE),
         `Outcome type` = factor(fam, levels = c("gaussian", "binomial"), labels = c("Continuous", "Binary")))
# I also added a dotted line for the true performance in each case
# and split out by binary and continuous outcome, 
estimates_plot <- plot_tib %>% 
  ggplot(aes(x = `Outcome relationship`, y = point_est, color = `Estimator`)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(data = true_perfs %>% filter(weak == simple_weak), 
             mapping = aes(yintercept = truth),
             linetype = "dashed") +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.8) +
  ylab("Estimated prediction performance (R-squared for continuous outcomes, AUC for binary outcomes)") +
  facet_grid(rows = vars(`Outcome type`), cols = vars(`X distribution`), labeller = label_both,
             scales = "free") +
  theme(panel.grid.major.y = element_line("gray85"), legend.position = "bottom",
        legend.direction = "horizontal")
ggsave(
  here("plots", "sim_performance_poster.png"),
  estimates_plot, width = 12, height = 7.5, dpi = 300
)
  
# # #Build a scatter-plot from lasso data
# bias_lasso_plot <- ggplot(data = lasso_sims %>% filter(!outlier)) +
#   geom_point(aes(x=cover_init, y=bias_init, color = linear), outlier.shape=NA) +
#   scale_color_viridis_d(option = "plasma") +
#   labs(x = "Q", y = "N") +
#   ggtitle("Lasso Bias")
# 
# # #Build a scatter-plot from superlearner data
# bias_superlearner_plot <- ggplot(data = superlearner_sims %>% filter(!outlier)) +
#   geom_point(aes(x=cover_init, y=bias_init, color = linear), outlier.shape=NA) +
#   scale_color_viridis_d(option = "plasma") +
#   labs(x = "Q", y = "N") +
#   ggtitle("SuperLearner Bias")
# 
# # ####Construct plot comparing Lasso and SL Bias####
# shared_legend <- get_legend(bias_lasso_plot + theme(legend.direction = "horizontal"))
# plot_grid(
#   plot_grid(bias_lasso_plot + theme(legend.position = "none"),
#             bias_superlearner_plot + theme(legend.position = "none")),
#   shared_legend, nrow = 2, ncol = 1, rel_heights = c(1, .1)
# )
