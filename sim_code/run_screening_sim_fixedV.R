#!usr/local/bin/Rscript

# investigate the use of variable screens within a Super Learner ensemble
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
# we consider a variety of data-generating mechanisms to do a comprehensive
# test of the methods, and consider varying restrictiveness of the screens

# ------------------------------------------------------------------------------
# load required functions and packages; do setup
# ------------------------------------------------------------------------------
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

parser <- OptionParser()
parser <- add_option(parser, "--sim-name", default = "continuous-strong-nonlinear-uncorrelated",
                    help = "the name of the simulation")
parser <- add_option(parser, "--nreps-total", type = "double", default = 1000,
                    help = "number of replicates in total")
parser <- add_option(parser, "--nreps-per-job", type = "double", default = 1,
                    help = "number of replicates for each job")
parser <- add_option(parser, "--est-type", default = "SL_screen",
                    help = "the estimator to run")
parser <- add_option(parser, "--job-id", default = 1, type = "double", # for n = 3000
                    help = "the job id (only used if running locally)")
parser <- add_option(parser, "--p", default = 10, type = "double", help = "the number of parameters (only used if running on Windows)")
parser <- add_option(parser, "--n", default = 200, type = "double", help = "the sample size (only used if running on Windows)")
parser <- add_option(parser, "--seed-mult", default = 1, type = "double", help = "multiplier for the random number seed")
args <- parse_args(parser, convert_hyphens_to_underscores = TRUE)

# pull in job id and set up command-line args
job_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if (is.na(job_id)) {
  # vimp package
  library("vimp")
  code_dir <- "sim_code/"
  prefix <- "sim_output/"
} else {
  # vimp package
  library("vimp", lib.loc = .libPaths()[2])
  code_dir <- "./"
  prefix <- paste0(Sys.getenv("DELETE90"), "/huang_y/bwillia2/sl_screening/")
}
source(paste0(code_dir, "gen_data.R"))
source(paste0(code_dir, "utils.R"))
source(paste0(code_dir, "sl_screens.R"))
source(paste0(code_dir, "do_one.R"))

print(paste0("Running sim ", args$sim_name, " with ", as.character(args$nreps_total),
             " total replicates and ", as.character(args$nreps_per_job),
             " replicates per job."))
print(paste0("Using ", args$est_type, " to do estimation."))

# set up all of the possible simulation parameters
ps <- c(10, 50, 100, 500)
ns <- c(200, 500, seq(1000, 3000, 1000))
# families <- c("gaussian", "binomial-logit", "binomial-probit")
families <- "gaussian"
if (grepl("binomial", args$sim_name)) {
  if (grepl("logit", args$sim_name)) {
    families <- "logit"
  } else {
    families <- "probit"
  }
}
nreps_per_combo <- args$nreps_total/args$nreps_per_job
param_grid <- expand.grid(mc_id = 1:nreps_per_combo, family = families, p = ps,
                          n = ns)

# grab the current simulation parameters
if (is.na(job_id)) {
  if (.Platform$OS.type == "windows") {
    job_id <- which((args$n == param_grid$n) & (args$p == param_grid$p))
  } else {
    job_id <- args$job_id
  }
}
current_dynamic_args <- param_grid[job_id, ]

# set up the beta vector
if (grepl("weak", args$sim_name)) {
  if (grepl("nonlinear", args$sim_name)) {
    active_set <- c(2, 3, 6)
  } else {
    active_set <- c(2, 6)
  }
  beta_active <- 1
  beta_0_active_set <- c(2, 6)
} else {
  active_set <- 1:6
  beta_active <- c(-3, -1, 1, -1.5, -0.5, 0.5)
  beta_0_active_set <- 1:6
}
active_set_with_intercept <- beta_0_active_set + 1
beta_0 <- c(0.5, rep(0, current_dynamic_args$p))
beta_0[active_set_with_intercept] <- beta_active

# create the SL library
xgb_tune_params <- list(max_depth = c(4), shrinkage = c(1e-2, 1e-1), ntrees = c(100, 500, 1000))
xgb_learners <- create.Learner("SL.xgboost_new", tune = xgb_tune_params,
                               detailed_names = TRUE, name_prefix = "xgb")
ranger_tune_params <- list(num.trees = c(1000), min.node.size = c(5, 20, 50, 100, 250))
rf_learners <- create.Learner("SL.ranger.imp", tune = ranger_tune_params,
                              detailed_names = TRUE, name_prefix = "rf")
earth_learners <- create.Learner("SL.earth", params = list(nk = min(max(21, 2 * current_dynamic_args$p + 1), 1000), Use.beta.cache = FALSE),
                                 detailed_names = FALSE, name_prefix = "earth")
if (current_dynamic_args$p > 10) {
  earth_learner <- earth_learners$names
} else {
  earth_learner <- "SL.earth"
}
# the learner library with no screens
learner_lib <- c(xgb_learners$names, rf_learners$names, "SL.glmnet", earth_learner)
if (grepl("no_lasso", args$est_type)) {
  learner_lib <- c(xgb_learners$names, rf_learners$names, earth_learner)
}
# if screens are requested (in the estimation procedure), add them
if (grepl("screen", args$est_type)) {
  # add screens to SL lib; the specific names are defined in sl_screens.R
  if (current_dynamic_args$p > 10) {
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
  if (grepl("screen_lasso", args$est_type)) {
    screens <- c(screens, "screen.glmnet")
  }
  if (grepl("only", args$est_type)) {
    screens <- "screen.glmnet"
  }
  learner_lib <- lapply(as.list(learner_lib), function(x) c(x, screens))
}

# ------------------------------------------------------------------------------
# replicate the simulation nreps_per_job times
# ------------------------------------------------------------------------------
current_seed <- job_id + (args$seed_mult - 1) * nrow(param_grid)
print(current_seed)
set.seed(current_seed)
if (.Platform$OS.type == "windows") {
  library("parallel")
  num_cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(num_cores)
  parallel::clusterExport(cl = cl, varlist = ls())
  parallel::clusterEvalQ(cl = cl, library("SuperLearner"))
  parallel::clusterEvalQ(cl = cl, library("ranger"))
  parallel::clusterEvalQ(cl = cl, library("xgboost"))
  parallel::clusterEvalQ(cl = cl, library("glmnet"))
  parallel::clusterEvalQ(cl = cl, library("earth"))
  parallel::clusterEvalQ(cl = cl, library("dplyr"))
  parallel::clusterEvalQ(cl = cl, library("tibble"))
  parallel::clusterEvalQ(cl = cl, library("data.table"))
  parallel::clusterEvalQ(cl = cl, library("vimp"))
  parallel::clusterEvalQ(cl = cl, library("cvAUC"))
  clusterSetRNGStream(cl = cl, iseed = current_seed)
  start <- Sys.time()
  sim_output <- parallel::parLapply(cl = cl,
                                    as.list(seq_len(args$nreps_per_job)),
                                    function(i) {
                                      do_one(
                                        iteration = i + args$nreps_per_job * (current_dynamic_args$mc_id - 1),
                                        n = current_dynamic_args$n, p = current_dynamic_args$p,
                                        family = switch((grepl("binomial", args$sim_name)) + 1,
                                                        gaussian(),
                                                        switch((grepl("probit", args$sim_name)) + 1,
                                                               binomial(link = "logit"), binomial(link = "probit"))),
                                        linear = !grepl("nonlinear", args$sim_name), beta = beta_0,
                                        estimator_type = args$est_type, learners = learner_lib,
                                        rho = ifelse(!grepl("uncorrelated", args$sim_name), 0.3, 0),
                                        rho2 = ifelse(!grepl("uncorrelated", args$sim_name),
                                                      ifelse(grepl("strong", args$sim_name), 0.9, 0.95), 0),
                                        corr_type = ifelse(!grepl("uncorrelated", args$sim_name), "complex", "easy"),
                                        active_set = active_set)
                                    }
  )
  end <- Sys.time()
  cat("Elapsed time: ", format(end - start), "\n")
  parallel::stopCluster(cl)
} else {
  start <- Sys.time()
  sim_output <- lapply(
    as.list(seq_len(args$nreps_per_job)),
    function(i) {
      do_one(
        iteration = i + args$nreps_per_job * (current_dynamic_args$mc_id - 1),
        n = current_dynamic_args$n, p = current_dynamic_args$p,
        family = switch((grepl("binomial", args$sim_name)) + 1,
                        gaussian(),
                        switch((grepl("probit", args$sim_name)) + 1,
                               binomial(link = "logit"), binomial(link = "probit"))),
        linear = !grepl("nonlinear", args$sim_name), beta = beta_0,
        estimator_type = args$est_type, learners = learner_lib,
        rho = ifelse(!grepl("uncorrelated", args$sim_name), 0.3, 0),
        rho2 = ifelse(!grepl("uncorrelated", args$sim_name),
                      ifelse(grepl("strong", args$sim_name), 0.9, 0.95), 0),
        corr_type = ifelse(!grepl("uncorrelated", args$sim_name), "complex", "easy"),
        active_set = active_set
      )
    }
  )
  end <- Sys.time()
  cat("Elapsed time: ", format(end - start), "\n")
}
sim_output_tib <- tibble::as_tibble(data.table::rbindlist(sim_output)) %>%
  mutate(weak = grepl("weak", args$sim_name), x_dist = "normal",
         corr = !grepl("uncorrelated", args$sim_name), .before = "est_type")
mn_truth <- mean(sim_output_tib$truth)
sim_output_tib %>%
  mutate(bias_init = sqrt(n) * (point_est - mn_truth),
         cover_init = cil <= mn_truth & ciu >= mn_truth) %>%
  group_by(n, p, fam, link, x_dist, linear, weak, corr, est_type) %>%
  summarize(bias = mean(bias_init, na.rm = TRUE), cover = mean(cover_init, na.rm = TRUE)) %>%
  print(n = Inf)
file_name <- paste0(args$est_type, "_id_", job_id, ".rds")
output_dir <- paste0(prefix, args$sim_name, "/")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
saveRDS(sim_output_tib, file = paste0(output_dir, file_name))
print("Analysis complete!")