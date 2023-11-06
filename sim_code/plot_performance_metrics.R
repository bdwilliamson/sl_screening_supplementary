# plot bias, variance, coverage

# load required packages and functions -----------------------------------------
library("readr")
library("dplyr")
library("tidyr")
library("tibble")
library("ggplot2")
library("cowplot")
theme_set(theme_cowplot())
library("here")

# read in the data -------------------------------------------------------------
# output is the result of a call to build_csv.R
output <- read_csv(here("sim_output", "sim_out.csv")) %>%
  filter(!is.na(p))

# add relevant columns ---------------------------------------------------------
true_perf <- output %>%
  group_by(fam, link, linear, weak, x_dist, corr) %>%
  summarize(truth = mean(truth), .groups = "drop")
output_fixed_truth <- output %>%
  select(-truth) %>%
  dplyr::left_join(true_perf, by = c("fam", "link", "linear", "weak", "x_dist", "corr"))
plot_tib <- output_fixed_truth %>%
  # filter(point_est > 0) %>%
  mutate(n_bias_init = sqrt(n) * (point_est - truth),
         bias_init = point_est - truth,
         cover_init = (cil <= truth) & (ciu >= truth),
         width_init = abs(ciu - cil)) %>%
  group_by(n, p, fam, link, linear, weak, x_dist, corr, est_type) %>%
  summarize(perf = mean(point_est, na.rm = TRUE),
            opt_perf = mean(truth, na.rm = TRUE),
            n_bias = mean(n_bias_init, na.rm = TRUE),
            bias = mean(bias_init, na.rm = TRUE),
            var_init = var(point_est, na.rm = TRUE),
            cover = mean(cover_init, na.rm = TRUE), 
            width = mean(width_init, na.rm = TRUE), .groups = "drop") %>%
  mutate(variance = n * var_init,
    base_estimator = factor(case_when(
      est_type == "lasso" ~ 1,
      grepl("SL_no_lasso", est_type) ~ 2,
      grepl("SL", est_type) & !grepl("SL_no_lasso", est_type) ~ 3
    ), levels = 1:3, labels = c("Lasso", "SL (-lasso)", "SL")),
    screens = factor(case_when(
      !grepl("screen", est_type) ~ 1,
      grepl("lasso_only", est_type) ~ 2,
      grepl("screen", est_type) & !grepl("screen_lasso", est_type) ~ 3,
      grepl("screen_lasso", est_type) ~ 4
    ), levels = 1:4, labels = c("None", "Lasso", "All (-lasso)", "All")),
  # est_fct = factor(case_when(
  #   est_type == "lasso" ~ 1,
  #   est_type == "SL" ~ 2,
  #   est_type == "SL_no_lasso" ~ 3,
  #   est_type == "SL_screen_lasso_only" ~ 4,
  #   est_type == "SL_no_lasso_screen_lasso_only" ~ 5,
  #   est_type == "SL_screen" ~ 6,
  #   est_type == "SL_no_lasso_screen" ~ 7,
  #   est_type == "SL_screen_lasso" ~ 8,
  #   est_type == "SL_no_lasso_screen_lasso" ~ 9
  # ), levels = 1:9, labels = c("Lasso", "SL", "SL (-lasso)",
  #                             "SL + lasso screen", "SL (-lasso) + lasso screen",
  #                             "SL + screens (-lasso)", "SL (-lasso) + screens (-lasso)",
  #                             "SL + screens (all)", "SL (-lasso) + screens (all)")),
  n_fct = factor(n), p = factor(p),
  `Outcome relationship` = factor(linear, levels = c(TRUE, FALSE), labels = c("Linear", "Nonlinear"),
                   ordered = TRUE),
  `X distribution` = factor(x_dist, levels = c("normal", "nonnormal"), labels = c("Normal", "Nonnormal"),
                 ordered = TRUE),
  `Strength of relationship` = factor(weak, levels = c(TRUE, FALSE), labels = c("Weak", "Strong"),
                                      ordered = TRUE),
  `Feature correlation` = factor(corr, levels = c(TRUE, FALSE), labels = c("Correlated", "Uncorrelated"),
                                 ordered = TRUE)) %>%
  select(-var_init)

# make the plots ---------------------------------------------------------------
all_ydists <- c("continuous", "binomial")
all_strengths <- c("Weak", "Strong")
fig_width <- 24
fig_height <- 12
for (i in 1:length(all_ydists)) {
  for (j in 1:length(all_strengths)) {
    this_ydist <- ifelse(all_ydists[i] == "continuous", "gaussian", "binomial")
    this_strength <- all_strengths[j]
    strength_text <- tolower(this_strength)
    this_plot_tib <- plot_tib %>%
      filter(fam == this_ydist, `Strength of relationship` == this_strength)
    # create a 4-panel plot
    bias_plot <- this_plot_tib %>%
      # ggplot(aes(x = n_fct, y = bias, shape = est_fct)) +
      ggplot(aes(x = n_fct, y = bias, shape = base_estimator, color = screens)) +
      geom_point(position = position_dodge(width = 0.75)) +
      scale_color_viridis_d(begin = 0, end = 0.5) +
      geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
      ylab(expression(paste(sqrt(n), " x empirical ", bias[n]))) +
      xlab("n") +
      ggtitle("BIAS") +
      labs(shape = "Estimator", color = "Screens") +
      geom_vline(aes(xintercept = 0.4), color = "grey85") +
      facet_grid(rows = vars(p), cols = vars(`Feature correlation`, `Outcome relationship`),
                 labeller = label_both, scales = "free")
    variance_plot <- this_plot_tib %>%
      ggplot(aes(x = n_fct, y = variance, shape = base_estimator, color = screens)) +
      geom_point(position = position_dodge(width = 0.75)) +
      scale_color_viridis_d(begin = 0, end = 0.5) +
      ylab(expression(paste(n, " x empirical ", variance[n]))) +
      xlab("n") +
      ggtitle("VARIANCE") +
      labs(shape = "Estimator", color = "Screens") +
      geom_vline(aes(xintercept = 0.4), color = "grey85") +
      facet_grid(rows = vars(p), cols = vars(`Feature correlation`, `Outcome relationship`),
                 labeller = label_both, scales = "free")
    cover_plot <- this_plot_tib %>%
      ggplot(aes(x = n_fct, y = cover, shape = base_estimator, color = screens)) +
      geom_point(position = position_dodge(width = 0.75)) +
      scale_color_viridis_d(begin = 0, end = 0.5) +
      geom_hline(aes(yintercept = 0.95), color = "red", linetype = "dashed") +
      ylab("Empirical coverage") +
      xlab("n") +
      ggtitle("COVERAGE") +
      labs(shape = "Estimator", color = "Screens") +
      geom_vline(aes(xintercept = 0.4), color = "grey85") +
      facet_grid(rows = vars(p), cols = vars(`Feature correlation`, `Outcome relationship`),
                 labeller = label_both, scales = "free")
    width_plot <- this_plot_tib %>%
      ggplot(aes(x = n_fct, y = width, shape = base_estimator, color = screens)) +
      geom_point(position = position_dodge(width = 0.75)) +
      scale_color_viridis_d(begin = 0, end = 0.5) +
      ylab("Confidence interval width") +
      xlab("n") +
      ggtitle("WIDTH") +
      labs(shape = "Estimator", color = "Screens") +
      geom_vline(aes(xintercept = 0.4), color = "grey85") +
      facet_grid(rows = vars(p), cols = vars(`Feature correlation`, `Outcome relationship`),
                 labeller = label_both, scales = "free")
    
    shared_legend <- get_legend(bias_plot +
                                  guides(shape = guide_legend(nrow = 1)) +
                                  theme(legend.position = "bottom"))
    four_panel_plot <- plot_grid(
      plot_grid(bias_plot + theme(legend.position = "none"),
                variance_plot + theme(legend.position = "none"),
                cover_plot + theme(legend.position = "none"),
                width_plot + theme(legend.position = "none"), labels = "AUTO"),
      shared_legend, nrow = 2, rel_heights = c(1, .1)
    )
    ggsave(here("plots", paste0("all-perf_", this_ydist, "_", strength_text, ".png")),
           plot = four_panel_plot, width = fig_width, height = fig_height, units = "in",
           dpi = 300)
    # create a four-panel prediction performance plot
    perf_plot_corr_linear <- this_plot_tib %>%
        filter(`Feature correlation` == "Correlated", `Outcome relationship` == "Linear") %>% 
        ggplot(aes(x = n_fct, y = perf, shape = base_estimator, color = screens)) +
        geom_hline(aes(yintercept = opt_perf), color = "red", linetype = "dashed") +
        geom_point(position = position_dodge(width = 0.75)) +
        scale_shape_manual(values = c(1, 17, 15)) +
        scale_color_viridis_d(begin = 0, end = 1) +
        ylab("Performance") +
        xlab("n") +
        ggtitle("Correlated, linear") +
        labs(shape = "Estimator", color = "Screens") +
        geom_vline(aes(xintercept = 0.4), color = "grey85") +
        facet_grid(rows = vars(p), labeller = label_both, scales = "free_y")
    perf_plot_corr_nonlinear <- this_plot_tib %>%
        filter(`Feature correlation` == "Correlated", `Outcome relationship` == "Nonlinear") %>% 
        ggplot(aes(x = n_fct, y = perf, shape = base_estimator, color = screens)) +
        geom_hline(aes(yintercept = opt_perf), color = "red", linetype = "dashed") +
        geom_point(position = position_dodge(width = 0.75)) +
        scale_shape_manual(values = c(1, 17, 15)) +
        scale_color_viridis_d(begin = 0, end = 1) +
        ylab("Performance") +
        xlab("n") +
        ggtitle("Correlated, nonlinear") +
        labs(shape = "Estimator", color = "Screens") +
        geom_vline(aes(xintercept = 0.4), color = "grey85") +
        facet_grid(rows = vars(p), labeller = label_both, scales = "free_y")
    perf_plot_uncorr_linear <- this_plot_tib %>%
        filter(`Feature correlation` == "Uncorrelated", `Outcome relationship` == "Linear") %>% 
        ggplot(aes(x = n_fct, y = perf, shape = base_estimator, color = screens)) +
        geom_hline(aes(yintercept = opt_perf), color = "red", linetype = "dashed") +
        geom_point(position = position_dodge(width = 0.75)) +
        scale_shape_manual(values = c(1, 17, 15)) +
        scale_color_viridis_d(begin = 0, end = 1) +
        ylab("Performance") +
        xlab("n") +
        ggtitle("Uncorrelated, linear") +
        labs(shape = "Estimator", color = "Screens") +
        geom_vline(aes(xintercept = 0.4), color = "grey85") +
        facet_grid(rows = vars(p), labeller = label_both, scales = "free_y")
    perf_plot_uncorr_nonlinear <- this_plot_tib %>%
        filter(`Feature correlation` == "Uncorrelated", `Outcome relationship` == "Nonlinear") %>% 
        ggplot(aes(x = n_fct, y = perf, shape = base_estimator, color = screens)) +
        geom_hline(aes(yintercept = opt_perf), color = "red", linetype = "dashed") +
        geom_point(position = position_dodge(width = 0.75)) +
        scale_shape_manual(values = c(1, 17, 15)) +
        scale_color_viridis_d(begin = 0, end = 1) +
        ylab("Performance") +
        xlab("n") +
        ggtitle("Uncorrelated, nonlinear") +
        labs(shape = "Estimator", color = "Screens") +
        geom_vline(aes(xintercept = 0.4), color = "grey85") +
        facet_grid(rows = vars(p), labeller = label_both, scales = "free_y")
    
    perf_legend <- get_legend(perf_plot_corr_linear +
                                  theme(legend.position = "bottom",
                                        legend.direction = "horizontal"))
    four_panel_perf_plot <- plot_grid(
        plot_grid(perf_plot_corr_linear + theme(legend.position = "none",
                                                axis.title.x = element_blank(),
                                                axis.text.x = element_blank(),
                                                strip.background = element_blank(),
                                                strip.text = element_blank()),
                  perf_plot_corr_nonlinear + theme(legend.position = "none",
                                                   axis.title.x = element_blank(),
                                                   axis.text.x = element_blank()),
                  perf_plot_uncorr_linear + theme(legend.position = "none",
                                                  strip.background = element_blank(),
                                                  strip.text = element_blank()),
                  perf_plot_uncorr_nonlinear + theme(legend.position = "none"),
                  nrow = 2, ncol = 2),
        perf_legend, nrow = 2, rel_heights = c(1, .1)
    )
    ggsave(here("plots", paste0("pred-perf_", this_ydist, "_", strength_text, ".png")),
           plot = four_panel_perf_plot, width = 8, height = 9, units = "in",
           dpi = 300)
  }
}

# simpler plots for the poster -------------------------------------------------
# streamline by selecting one dimension (p), comparing nonlinear to linear outcome relationship
# still want both continuous and binary Y
# choose strong relationship (since weak just reduces performance across the board)
simple_p <- 500
simple_strength <- "Strong"
simple_xdist <- "normal"
simple_plot_tib <- plot_tib %>%
  filter(`Strength of relationship` == simple_strength,
         p == simple_p, x_dist == simple_xdist) %>%
  mutate(fam_fct = factor(fam, levels = c("gaussian", "binomial"), labels = c("Continuous", "Binary"))) %>%
  rename(`Outcome type` = fam_fct)
# create a six-panel plot: bias, variance, coverage (cols) for both continuous and binary Y (rows)
point_size <- 2
text_size <- 18
axis_text_size <- 16
bias_plot <- simple_plot_tib %>%
  ggplot(aes(x = n_fct, y = bias, shape = base_estimator, color = screens)) +
  geom_point(position = position_dodge(width = 0.75), size = point_size) +
  scale_color_viridis_d(begin = 0, end = 0.75) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
  ylab(expression(paste(sqrt(n), " x empirical ", bias[n]))) +
  xlab("n") +
  ggtitle("BIAS") +
  labs(shape = "Estimator") +
  facet_grid(rows = vars(`Outcome type`), cols = vars(`Outcome relationship`, `Feature correlation`),
             labeller = label_both, scales = "free")
variance_plot <- simple_plot_tib %>%
  ggplot(aes(x = n_fct, y = variance, shape = est_fct)) +
  geom_point(position = position_dodge(width = 0.75), size = point_size) +
  ylab(expression(paste(n, " x empirical ", variance[n]))) +
  xlab("n") +
  ggtitle("VARIANCE") +
  labs(shape = "Estimator") +
  facet_grid(rows = vars(`Outcome type`), cols = vars(`Outcome relationship`),
             labeller = label_both, scales = "free")
cover_plot <- simple_plot_tib %>%
  ggplot(aes(x = n_fct, y = cover, shape = est_fct)) +
  geom_point(position = position_dodge(width = 0.75), size = point_size) +
  geom_hline(aes(yintercept = 0.95), color = "red", linetype = "dashed") +
  ylab("Empirical coverage") +
  xlab("n") +
  ggtitle("COVERAGE") +
  labs(shape = "Estimator") +
  facet_grid(rows = vars(`Outcome type`), cols = vars(`Outcome relationship`),
             labeller = label_both, scales = "free")
shared_legend <- get_legend(bias_plot +
                            guides(shape = guide_legend(nrow = 1)) +
                            theme(legend.position = "bottom",
                                  legend.text = element_text(size = axis_text_size),
                                  legend.title = element_text(size = text_size)))
six_panel_plot <- plot_grid(
  plot_grid(bias_plot + theme(legend.position = "none", text = element_text(size = text_size),
                              axis.text = element_text(size = axis_text_size)),
            variance_plot + theme(legend.position = "none", text = element_text(size = text_size),
                                  axis.text = element_text(size = axis_text_size)),
            cover_plot + theme(legend.position = "none", text = element_text(size = text_size),
                               axis.text = element_text(size = axis_text_size)),
            labels = "AUTO", nrow = 1),
  shared_legend, nrow = 2, rel_heights = c(1, .1)
)
ggsave(here("plots", paste0("pred-perf_poster.png")),
       plot = six_panel_plot, width = fig_width, height = fig_height, units = "in",
       dpi = 300)
