# plots and tables with results from data analysis

# load required functions and packages -----------------------------------------
library("dplyr")
library("tibble")
library("knitr")
library("kableExtra")
library("rprojroot") # for trajectory traversing
this_path <- normalizePath(".", mustWork = FALSE)
proj_root <- rprojroot::find_root_file(criterion = ".projectile", path = this_path)

source(paste0(proj_root, "/sim_code/00_utils.R"))

# read in prediction performance estimates -------------------------------------
output_dir <- paste0(proj_root, "/results/")
output <- readRDS(file = paste0(output_dir, "est_risks.rds")) %>% 
  mutate(Algorithm = case_when(Algorithm == "Super Learner" ~ "cSL",
                               Algorithm == "Discrete SL" ~ "dSL",
                               Algorithm == "SL.glmnet_All" ~ "lasso",
                               !(Algorithm %in% c("Super Learner", "Discrete SL", "SL.glmnet_All")) ~ Algorithm))

font_size <- 9
# create nice tables of results ------------------------------------------------
# Main tables 1 and 2: R-squared (continuous) and AUC (binary)
# cSL and dSL for all screener libraries, lasso
main_algos <- c("cSL", "dSL", "lasso")
main_tab_results <- output %>% 
  filter(Algorithm %in% main_algos, risk_type == "main") %>% 
  filter(learners == "All") %>% 
  filter(!(Algorithm == "lasso" & !(screeners == "None"))) %>% 
  mutate(est_ci = sprintf("%.3f [%.3f, %.3f]", point_est, cil, ciu)) %>% 
  select(-log_se, -risk_type, -outcome_type, -se, -log_se, -point_est, -cil, -ciu) %>% 
  select(learners, screeners, Algorithm, Min, Max, est_ci, risk_measure) %>% 
  rename(Learners = learners, Screeners = screeners, `Point estimate [95\\% CI]` = est_ci)

# print out a table for continuous outcomes
main_tab_results %>% 
  filter(risk_measure == "R-squared") %>% 
  select(-risk_measure) %>% 
  knitr::kable(digits = 3, format = "latex", booktabs = TRUE, escape = FALSE,
               caption = paste0("Estimates of cross-validated R-squared for the ",
                                "continuous $\\text{IC}_{50}$ outcome, for the ",
                                "convex ensemble super learner (cSL), the ",
                                "discrete super learner (dSL), and the lasso, ",
                                "under each combination of learners and screeners. ",
                                "For screeners, `None' denotes no screeners; ",
                                "`Lasso' denotes only a lasso screener; ",
                                "`All (-lasso)' denotes random forest, rank-correlation, and correlation-test p-value screening; ",
                                "`All' denotes these three screener types plus the lasso; and ",
                                "`All (+none)' denotes all screeners plus the 'none' screener.",
                                "\\label{tab:data_analysis_main_continuous}"), 
               linesep = "") %>% 
  kableExtra::kable_styling(font_size = font_size, latex_options = c("scale_down")) %>% 
  kableExtra::save_kable(file = paste0(output_dir, "data_analysis_main_continuous.tex"))
  
# print out a table for binary outcomes
main_tab_results %>% 
  filter(risk_measure == "AUC") %>% 
  select(-risk_measure) %>% 
  knitr::kable(digits = 3, format = "latex", booktabs = TRUE, escape = FALSE,
               caption = paste0("Estimates of cross-validated AUC for the ",
                                "binary sensitivity outcome, for the ",
                                "convex ensemble super learner (cSL), the ",
                                "discrete super learner (dSL), and the lasso, ",
                                "under each combination of learners and screeners. ",
                                "For screeners, `None' denotes no screeners; ",
                                "`Lasso' denotes only a lasso screener; ",
                                "`All (-lasso)' denotes random forest, rank-correlation, and correlation-test p-value screening; ",
                                "`All' denotes these three screener types plus the lasso; and ",
                                "`All (+none)' denotes all screeners plus the 'none' screener.",
                                "\\label{tab:data_analysis_main_binary}"), 
               linesep = "") %>% 
  kableExtra::kable_styling(font_size = font_size, latex_options = c("scale_down")) %>% 
  kableExtra::save_kable(file = paste0(output_dir, "data_analysis_main_binary.tex"))


# Supplementary tables:
# Supp table 1: log likelihood for all cSL, dSL and lasso from above
supp_nll_results <- output %>% 
  filter(Algorithm %in% main_algos, risk_type == "matched") %>% 
  filter(learners == "All") %>% 
  filter(!(Algorithm == "lasso" & !(screeners == "None"))) %>% 
  mutate(est_ci = ifelse(!is.na(se) | !is.na(log_se), sprintf("%.3f [%.3f, %.3f]", point_est, cil, ciu), sprintf("%.3f", point_est))) %>% 
  select(-log_se, -risk_type, -outcome_type, -se, -log_se, -point_est, -cil, -ciu) %>% 
  select(learners, screeners, Algorithm, Min, Max, est_ci, risk_measure) %>% 
  rename(Learners = learners, Screeners = screeners, `Point estimate` = est_ci)

supp_nll_results %>% 
  filter(risk_measure == "Negative log likelihood") %>% 
  select(-risk_measure) %>% 
  knitr::kable(digits = 3, format = "latex", booktabs = TRUE, escape = FALSE,
               caption = paste0("Estimates of cross-validated negative log likelihood for the ",
                                "binary sensitivity outcome, for the ",
                                "convex ensemble super learner (cSL), the ",
                                "discrete super learner (dSL), and the lasso, ",
                                "under each combination of learners and screeners. ",
                                "For screeners, `None' denotes no screeners; ",
                                "`Lasso' denotes only a lasso screener; ",
                                "`All (-lasso)' denotes random forest, rank-correlation, and correlation-test p-value screening; ",
                                "`All' denotes these three screener types plus the lasso; and ",
                                "`All (+none)' denotes all screeners plus the 'none' screener.",
                                "\\label{tab:data_analysis_supp_binary}"), 
               linesep = "") %>% 
  kableExtra::kable_styling(font_size = font_size, latex_options = c("scale_down")) %>% 
  kableExtra::save_kable(file = paste0(output_dir, "data_analysis_supp_binary.tex"))

# Supp table 2: Prediction performance for all screener-learner combos
supp_algo_results <- output %>% 
  filter(!(Algorithm %in% main_algos)) %>% 
  filter(learners == "All") %>% 
  filter(!(Algorithm == "lasso" & !(screeners == "None"))) %>% 
  mutate(est_ci = ifelse(!is.na(se) | !is.na(log_se), 
                         ifelse(point_est < -10, "< -10", sprintf("%.3f [%.3f, %.3f]", point_est, cil, ciu)), 
                         ifelse(point_est < -10, "< -10", sprintf("%.3f", point_est))),
         Min = ifelse(Min < -10, "< -10", round(Min, 3)),
         Max = ifelse(Max < -10, "< -10", round(Max, 3))) %>% 
  select(-log_se, -risk_type, -outcome_type, -se, -log_se, -point_est, -cil, -ciu) %>% 
  select(learners, screeners, Algorithm, Min, Max, est_ci, risk_measure) %>% 
  rename(Learners = learners, Screeners = screeners, `Point estimate [95\\% CI]` = est_ci) %>% 
  mutate(Algorithm = gsub("_", "\\_", Algorithm, fixed = TRUE))

supp_algo_results %>% 
  rename(`Risk measure` = risk_measure) %>% 
  knitr::kable(digits = 3, format = "latex", booktabs = TRUE, longtable = TRUE, escape = FALSE,
               caption = paste0("Estimates of cross-validated R-squared (continuous outcome), ", "
                                AUC and negative log likelihood (binary outcome) for the ",
                                "learner-screener pairs ",
                                "under each combination of learners and screeners. ",
                                "For screeners, `None' denotes no screeners; ",
                                "`Lasso' denotes only a lasso screener; ",
                                "`All (-lasso)' denotes random forest, rank-correlation, and correlation-test p-value screening; ",
                                "`All' denotes these three screener types plus the lasso; and ",
                                "`All (+none)' denotes all screeners plus the `none' screener.",
                                "\\label{tab:data_analysis_supp_all}"), 
               linesep = "") %>% 
  kableExtra::kable_styling(font_size = font_size, 
                            latex_options = c("repeat_header"),
                            repeat_header_text = "Estimates of cross-validated prediction performance for all screener-learner pairs within the super learner. \\textit{(continued)}",
                            repeat_header_method = "replace") %>% 
  kableExtra::save_kable(file = paste0(output_dir, "data_analysis_supp_all.tex"))

# read in fitted CV.SL objects, print out the coefficients
fit_continuous <- readRDS(paste0(output_dir, "fit_ic50.rds"))
fit_binary <- readRDS(paste0(output_dir, "fit_sens50.rds"))


fit_continuous_coef_dsl <- cbind.data.frame(fit_continuous$coef, dSL = unlist(lapply(fit_continuous$whichDiscreteSL, get_learner_screener))) %>% 
  mutate(across(where(is.numeric), 
                .fns = ~ ifelse(.x < 0.001 & .x > 0, "< 0.001", as.character(round(.x, 3))))) %>% 
  t() %>%
  as.data.frame() %>% 
  tibble::rownames_to_column() 
continuous_learners <- strsplit(fit_continuous_coef_dsl$rowname, "_", fixed = TRUE)
fit_continuous_coef_dsl$Learner <- unlist(lapply(continuous_learners, function(x) gsub("SL.", "", x[1])))
fit_continuous_coef_dsl$Screener <- unlist(lapply(continuous_learners, function(x) {
  ifelse(!any(grepl("screen.", x)), "", gsub("screen.", "", x[grep("screen.", x, fixed = TRUE)], fixed = TRUE))
}))

fit_continuous_coef_dsl %>% 
  select(Learner, Screener, `1`:`10`) %>% 
  knitr::kable(digits = 3, format = "latex", booktabs = TRUE, escape = FALSE,
               caption = paste0("$\\text{IC}_{50}$: Ensemble super learner (cSL) coefficients for each learner-screener pair, ",
                                "for each of the 10 outer cross-validation folds, along with the ",
                                "algorithm selected as the discrete super learner (dSL).",
                                "\\label{tab:data_analysis_supp_continuous_coefs_dsl}"),
               linesep = "") %>% 
  kableExtra::kable_styling(font_size = font_size, latex_options = c("scale_down")) %>% 
  kableExtra::column_spec(1:10, width = "5em") %>% 
  kableExtra::save_kable(file = paste0(output_dir, "data_analysis_supp_continuous_coefs_dsl.tex"))

fit_binary_coef_dsl <- cbind.data.frame(fit_binary$coef, dSL = unlist(lapply(fit_binary$whichDiscreteSL, get_learner_screener))) %>% 
  mutate(across(where(is.numeric), 
                .fns = ~ ifelse(.x < 0.001 & .x > 0, "< 0.001", as.character(round(.x, 3))))) %>% 
  t() %>%
  as.data.frame() %>% 
  tibble::rownames_to_column() 
binary_learners <- strsplit(fit_binary_coef_dsl$rowname, "_", fixed = TRUE)
fit_binary_coef_dsl$Learner <- unlist(lapply(binary_learners, function(x) gsub("SL.", "", x[1])))
fit_binary_coef_dsl$Screener <- unlist(lapply(binary_learners, function(x) {
  ifelse(!any(grepl("screen.", x)), "", gsub("screen.", "", x[grep("screen.", x, fixed = TRUE)], fixed = TRUE))
}))
fit_binary_coef_dsl %>% 
  select(Learner, Screener, `1`:`10`) %>% 
  knitr::kable(digits = 3, format = "latex", booktabs = TRUE, escape = FALSE,
               caption = paste0("Susceptibility: Ensemble super learner (cSL) coefficients for each learner-screener pair, ",
                                "for each of the 10 outer cross-validation folds, along with the ",
                                "algorithm selected as the discrete super learner (dSL).",
                                "\\label{tab:data_analysis_supp_binary_coefs_dsl}"),
               linesep = "") %>% 
  kableExtra::kable_styling(font_size = font_size, latex_options = c("scale_down")) %>% 
  kableExtra::column_spec(1:10, width = "5em") %>% 
  kableExtra::save_kable(file = paste0(output_dir, "data_analysis_supp_binary_coefs_dsl.tex"))
