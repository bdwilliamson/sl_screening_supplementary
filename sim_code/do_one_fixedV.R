# run the simulation a single time
# @param iteration the iteration number (out of 1:nreps_total)
# @param n the sample size
# @param p the number of features
# @param family the family, determines whether the outcome is continuous or binary
# @param linear is Y | X a linear model or not?
# @param rho correlation parameter
# @param rho2 second correlation parameter
# @param corr_type type of correlation (complex or simple/none?)
# @param estimator_type the type of estimator (e.g., "sl")
# @param learners the SuperLearner library
do_one <- function(iteration = 1, n = 200, p = 50, family = gaussian(),
                   linear = TRUE, rho = 0, rho2 = 0, active_set = 1:6,
                   corr_type = "simple", estimator_type = "sl",
                   beta = rep(0, p), learners = c("SL.glmnet", "SL.ranger.imp")) {
  # generate data according to the specification
  if (linear) {
    data_gen <- gen_data_lm
    true_conditional_mean <- function(x, beta, xdist, ydist) cbind(1, x) %*% as.matrix(beta)
  } else {
    data_gen <- gen_data_nlm
    true_conditional_mean <- function(x, beta, xdist, ydist) {
      nl_conditional_mean(x, as.matrix(beta), x_dist = xdist, y_dist = "many")
    }
  }
  dat <- data_gen(n = n, p = p, rho = rho, rho2 = rho2, beta = matrix(beta), family = family,
                  x_dist = "normal", corr_type = corr_type, active_set = active_set,
                  y_dist = "many")
  # generate a *large* test dataset according to the same specification
  test_dat <- data_gen(n = 1e6, p = p, rho = rho, rho2 = rho2, beta = matrix(beta), family = family,
                       x_dist = "normal", corr_type = corr_type, active_set = active_set,
                       y_dist = "many")
  y <- dat %>% pull(y)
  x <- dat %>% select(-y)
  # generate folds for cross-validation
  V <- 5
  folds <- sample(seq_len(V), size = nrow(x), replace = TRUE)
  point_ests <- vector("numeric", length = V)
  vars <- vector("numeric", length = V)
  for (v in seq_len(V)) {
    x_train <- x[folds != v, ]
    x_test <- x[folds == v, ]
    y_train <- y[folds != v]
    y_test <- y[folds == v]
    # fit the estimator:
    #   if estimator_type == "lasso", fit that (for comparison)
    #   otherwise, fit a Super Learner
    if (estimator_type == "lasso") {
      fit <- glmnet::cv.glmnet(x = as.matrix(x_train), y = y_train, nfolds = 10, intercept = TRUE,
                               family = family)
      preds <- predict(fit, newx = as.matrix(x_test), s = "lambda.min")
    } else {
      sl_opts <- get_sl_opts(family)
      fit <- tryCatch(
        SuperLearner::SuperLearner(Y = y_train, X = x_train, SL.library = learners,
                                        cvControl = list(V = 5, stratifyCV = (sl_opts$fam$family == "binomial")),
                                        family = sl_opts$fam, method = sl_opts$method),
        error = function(e) {
          SuperLearner::SuperLearner(Y = y_train, X = x_train, SL.library = learners,
                                        cvControl = list(V = 5, stratifyCV = (sl_opts$fam$family == "binomial")),
                                        family = sl_opts$fam, method = "method.NNLS")
        }
      )
      preds <- SuperLearner::predict.SuperLearner(fit, newdata = x_test, onlySL = TRUE)$pred
    }
    # remaining items:
    # (1) estimate performance, using CV-R-squared for continuous outcomes and CV-AUC for binary outcomes
    if (family$family == "gaussian") {
      est_perf <- vimp::measure_r_squared(fitted_values = preds, y = y_test)
    } else {
      est_perf <- vimp::measure_auc(fitted_values = preds, y = y_test)
    }
    point_ests[v] <- est_perf$point_est
    vars[v] <- mean((est_perf$eif) ^ 2)
  }
  point_est <- mean(point_ests)
  se <- sqrt(mean(vars) / n)
  ci <- vimp::vimp_ci(point_est, se)
  # compute the true value of performance
  true_linear_predictor <- true_conditional_mean(x = as.matrix(test_dat %>% select(-y)),
                                                 beta = beta, xdist = "normal")
  test_y <- test_dat %>% pull(y)
  if (family$family == "gaussian") {
    true_perf <- vimp::measure_r_squared(fitted_values = true_linear_predictor, y = test_y)$point_est
  } else {
    true_perf <- vimp::measure_auc(fitted_values = true_linear_predictor, y = test_y)$point_est
  }
  # (2) return a nice tibble
  output <- tibble::tibble(
    mc_id = iteration, n = n, p = p, fam = family$family, link = family$link,
    linear = linear, est_type = estimator_type, truth = true_perf, point_est = point_est,
    se = se, cil = ci[, 1], ciu = ci[, 2]
  )
  output
}
