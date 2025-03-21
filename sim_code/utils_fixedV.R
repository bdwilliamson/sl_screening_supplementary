# utility functions

# utility functions for super learner
get_sl_opts <- function(family) {
  if (is.character(family)) {
    family <- get(family, mode = "function", envir = parent.frame())
  }
  if (family$family == "gaussian") {
    sl_fam <- gaussian()
    sl_method <- "method.CC_LS"
  } else {
    if (grepl("probit", family$link)) {
      sl_fam <- binomial(link = "probit")
    } else {
      sl_fam <- binomial()
    }
    sl_method <- "method.CC_nloglik"
  }
  return(list(fam = sl_fam, method = sl_method))
}

# ------------------------------------------------------------------------------
# updates to existing super learner functions (these updates either fix
# warning messages or return extra output)
# ------------------------------------------------------------------------------
# boosted trees
SL.xgboost_new <- function (Y, X, newX, family, obsWeights, id, ntrees = 1000,
    max_depth = 4, shrinkage = 0.1, minobspernode = 10, params = list(),
    nthread = 1, verbose = 0, save_period = NULL, ...)
{
    if (!is.matrix(X)) {
        X = model.matrix(~. - 1, X)
    }
    xgmat = xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)
    if (family$family == "gaussian") {
        model = xgboost::xgboost(data = xgmat, objective = "reg:squarederror", ## if xgboost version >=1.1.1.1, changed from reg:linear to reg:squarederror
            nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode,
            eta = shrinkage, verbose = verbose, nthread = nthread,
            params = params, save_period = save_period)
    }
    if (family$family == "binomial") {
        model = xgboost::xgboost(data = xgmat, objective = "binary:logistic",
            nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode,
            eta = shrinkage, verbose = verbose, nthread = nthread,
            params = params, save_period = save_period)
    }
    if (family$family == "multinomial") {
        model = xgboost::xgboost(data = xgmat, objective = "multi:softmax",
            nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode,
            eta = shrinkage, verbose = verbose, num_class = length(unique(Y)),
            nthread = nthread, params = params, save_period = save_period)
    }
    if (!is.matrix(newX)) {
        newX = model.matrix(~. - 1, newX)
    }
    pred = predict(model, newdata = newX)
    fit = list(object = model)
    class(fit) = c("SL.xgboost")
    out = list(pred = pred, fit = fit)
    return(out)
}

# random forests
SL.ranger.imp <- function (Y, X, newX, family, obsWeights = rep(1, length(Y)),
                           num.trees = 500, mtry = floor(sqrt(ncol(X))),
                           write.forest = TRUE, probability = family$family == "binomial",
                           min.node.size = ifelse(family$family == "gaussian", 5, 1),
                           replace = TRUE, sample.fraction = ifelse(replace, 1, 0.632),
                           num.threads = 1, verbose = FALSE, ...) {
  SuperLearner:::.SL.require("ranger")
  if (family$family == "binomial") {
    Y = as.factor(Y)
  }
  if (is.matrix(X)) {
    X = data.frame(X)
  }
  fit <- ranger::ranger(`_Y` ~ ., data = cbind(`_Y` = Y, X),
                        num.trees = num.trees, mtry = mtry, min.node.size = min.node.size,
                        replace = replace, sample.fraction = sample.fraction,
                        case.weights = obsWeights, write.forest = write.forest,
                        probability = probability, num.threads = num.threads,
                        verbose = verbose, importance = "impurity")
  pred <- predict(fit, data = newX)$predictions
  if (family$family == "binomial") {
    pred = pred[, "1"]
  }
  fit <- list(object = fit, verbose = verbose)
  class(fit) <- c("SL.ranger")
  out <- list(pred = pred, fit = fit)
  return(out)
}
SL.ranger.reg <- function(..., X, mtry = floor(sqrt(ncol(X)))) {
  SL.ranger.imp(..., X = X, mtry = mtry)
}
SL.ranger.small <- function(..., X, mtry = floor(sqrt(ncol(X)) * 1/2)) {
  SL.ranger.imp(..., X = X, mtry = mtry)
}
SL.ranger.large <- function(..., X, mtry = floor(sqrt(ncol(X)) * 2)) {
  SL.ranger.imp(..., X = X, mtry = mtry)
}