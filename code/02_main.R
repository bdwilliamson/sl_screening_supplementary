#!usr/local/bin/Rscript

# investigate the use of variable screens within a Super Learner ensemble
# using VRC01 data from CATNAP
# we consider the following estimation procedures:
#   (a) lasso alone
#   (b) SL with no variable screens
#     (i) library contains lasso
#     (ii) library doesn't contain lasso
#   (c) SL with variable screens
#     (i) screen library contains lasso
#       (1) candidate library contains lasso
#       (2) candidate library doesn't contain lasso
#     (ii) screen library doesn't contain lasso
#       (1) candidate library contains lasso
#       (2) candidate library doesn't contain lasso

# load required functions and packages -----------------------------------------
# load in tidyr, dplyr, stringr, tibble (and others)
library("tidyverse")
library("data.table")
# SL, plus lasso, xgboost, random forests; routines for optimization
library("SuperLearner")
library("glmnet")
library("xgboost")
library("ranger")
library("earth")
library("nloptr")
library("quadprog")
# parsing command-line args
library("optparse")
library("methods")
# compute cross-validated AUC
library("cvAUC")
library("rprojroot") # for trajectory traversing
this_path <- normalizePath(".", mustWork = FALSE)
proj_root <- rprojroot::find_root_file(criterion = ".projectile", path = this_path)

code_dir <- paste0(proj_root, "/code/")
sim_code_dir <- paste0(proj_root, "/sim_code/")
output_dir <- paste0(proj_root, "/results/")
source(paste0(sim_code_dir, "00_sl_screens.R"))
source(paste0(sim_code_dir, "00_utils.R"))

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# load the dataset -------------------------------------------------------------
data_dir <- paste0(proj_root, "/data/")
vrc01_data <- readRDS(paste0(data_dir, "vrc01_data.rds"))
n <- nrow(vrc01_data)

# remove unnecessary columns
vrc01_data_reduced <- vrc01_data %>% 
  select(-contains("seqname"), -contains("ic80"),
         -contains(".raw"), -contains("censored"), -ic50.geometric.mean.imputed,
         -contains("coreceptor"), -infection.stage.ordinal, -tier.ordinal,
         -neutralization.slope, -binding.dichotomous.sens.resis, -gb.accession,
         -country.of.origin, -geographic.region.of.origin, -subtype, -subtype.reduced,
         -sens80)

dat_binary <- vrc01_data_reduced %>% 
  select(-contains("ic50"))

dat_continuous <- vrc01_data_reduced %>% 
  select(-sens50)

p <- ncol(dat_binary) - 1

# set up arguments for Super Learner -------------------------------------------
# get effective sample size for continuous and binary outcome
n_eff_continuous <- get_n_eff(n = n, outcome_type = "continuous")
n_rare <- min(sum(vrc01_data$sens50), sum(1 - vrc01_data$sens50))
n_eff_binary <- get_n_eff(n = n, n_rare = n_rare, outcome_type = "binary")

V_continuous <- setV(n.effective = n_eff_continuous)
V_binary <- setV(n.effective = n_eff_binary)

# create the SL library; note nk decreased for computational reasons (did not affect convergence)
earth_learners <- create.Learner("SL.earth", params = list(nk = min(max(21, 2 * p + 1), 200), Use.beta.cache = FALSE),
                                 detailed_names = FALSE, name_prefix = "earth")
if (p > 10) {
  earth_learner <- earth_learners$names
} else {
  earth_learner <- "SL.earth"
}
# the full candidate learner library
all_candidate_learners <- c("SL.glm", "SL.ranger", "SL.glmnet", earth_learner)
# the full candidate set of screeners
if (p > 10) {
  screens <- c(paste0("screen.corRank.", c("10", "25", "50")),
               paste0("screen.corP.", c("20", "40")),
               paste0("screen.ranger.", c("10", "25"))
  )
} else {
  screens <- c(paste0("screen.corRank.", c("10")),
               paste0("screen.corP.", c("20")),
               paste0("screen.ranger.", c("10"))
  )
}
all_candidate_screeners <- c(screens, "screen.glmnet")
# only add "all" for glmnet
candidate_learner_screener_lib <- lapply(as.list(all_candidate_learners), function(x) {
  if (grepl("glmnet", x)) {
    c(x, c("All"))
  } else {
    c(x, c("All", all_candidate_screeners))  
  }
}
)

# run the Super Learner for the continuous outcome -----------------------------
sl_opts_continuous <- get_sl_opts(family = gaussian())
# switch method to NNLS due to failure in CC_LS
sl_opts_continuous$method <- "method.NNLS"
set.seed(20240920)
fit_continuous <- SuperLearner::CV.SuperLearner(
  Y = dat_continuous$ic50.geometric.mean.imputed.log10, 
  X = dat_continuous %>% select(-ic50.geometric.mean.imputed.log10),
  SL.library = candidate_learner_screener_lib,
  method = sl_opts_continuous$method, family = sl_opts_continuous$fam,
  cvControl = list(V = V_continuous), innerCvControl = rep(list(list(V = V_continuous)), V_continuous)
)

# run the Super Learner for the binary outcome ---------------------------------
sl_opts_binary <- get_sl_opts(family = binomial())
set.seed(20240920)
fit_binary <- SuperLearner::CV.SuperLearner(
  Y = dat_binary$sens50, 
  X = dat_binary %>% select(-sens50),
  SL.library = candidate_learner_screener_lib,
  method = sl_opts_binary$method, family = sl_opts_binary$fam,
  cvControl = list(V = V_binary, stratifyCV = TRUE), 
  innerCvControl = rep(list(list(V = V_binary, stratifyCV = TRUE)), V_binary)
)

# compute cross-validated prediction performance -------------------------------
# get CV risk
risk_matched_continuous <- get_risk_all_estimators(cv_sl_fit = fit_continuous,
                                                   method = "method.NNLS")
risk_main_continuous <- risk_matched_continuous

risk_matched_binary <- get_risk_all_estimators(cv_sl_fit = fit_binary,
                                               method = "method.NNloglik")
risk_main_binary <- get_risk_all_estimators(cv_sl_fit = fit_binary,
                                            method = "method.AUC")
# add on CIs
risk_matched_continuous_ci <- get_log_ci(est = risk_matched_continuous$Ave, log_se = risk_matched_continuous$log_se)
risk_main_continuous_ci <- risk_matched_continuous_ci

risk_matched_binary_ci <- risk_matched_binary$Ave + risk_matched_binary$se %o% qnorm(c(0.025, 0.975))
risk_main_binary_ci <- risk_main_binary$Ave + risk_main_binary$se %o% qnorm(c(0.025, 0.975))

risk_matched_continuous$cil <- risk_matched_continuous_ci[, 1]
risk_matched_continuous$ciu <- risk_matched_continuous_ci[, 2]
risk_main_continuous$cil <- risk_main_continuous_ci[, 1]
risk_main_continuous$ciu <- risk_main_continuous_ci[, 2]

risk_matched_binary$cil <- risk_matched_binary_ci[, 1]
risk_matched_binary$ciu <- risk_matched_binary_ci[, 2]
risk_main_binary$cil <- risk_main_binary_ci[, 1]
risk_main_binary$ciu <- risk_main_binary_ci[, 2]

# combine together
output <- bind_rows(
  risk_matched_continuous %>% mutate(risk_type = "matched", 
                                     outcome_type = "continuous",
                                     risk_measure = "R-squared"), 
  risk_main_continuous %>% mutate(risk_type = "main",
                                  outcome_type = "continuous",
                                  risk_measure = "R-squared"),
  risk_matched_binary %>% mutate(risk_type = "matched",
                                 outcome_type = "binary",
                                 risk_measure = "Negative log likelihood"),
  risk_main_binary %>% mutate(risk_type = "main",
                              outcome_type = "binary",
                              risk_measure = "AUC")
) %>% 
  rename(point_est = Ave)
# save results -----------------------------------------------------------------
saveRDS(fit_continuous, file = paste0(output_dir, "fit_ic50.rds"))
saveRDS(fit_binary, file = paste0(output_dir, "fit_sens50.rds"))
saveRDS(output, file = paste0(output_dir, "est_risks.rds"))
