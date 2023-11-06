# variable screening algorithms for the SL screening simulation project

# ------------------------------------------------------------------------------
# screen based on rank correlation with varying strictness
# ------------------------------------------------------------------------------
# @param Y the outcome
# @param X the features
# @param family the distribution/link function to use (e.g., "gaussian")
# @param method the method for correlation ("spearman")
# @param rank the number of features to choose
# @param ... other args to screen.corRank (part of the SuperLearner package)
screen.corRank.4 <- function(Y, X, family, method = "spearman", rank = 4, ...) {
  screen.corRank(Y = Y, X = X, family = family, method = method, rank = rank, ...)
}
screen.corRank.10 <- function(Y, X, family, method = "spearman", rank = 10, ...) {
  screen.corRank(Y = Y, X = X, family = family, method = method, rank = rank, ...)
}
screen.corRank.25 <- function(Y, X, family, method = "spearman", rank = 25, ...) {
  screen.corRank(Y = Y, X = X, family = family, method = method, rank = rank, ...)
}
screen.corRank.50 <- function(Y, X, family, method = "spearman", rank = 50, ...) {
  screen.corRank(Y = Y, X = X, family = family, method = method, rank = rank, ...)
}
# ------------------------------------------------------------------------------
# screen based on univariate correlation with varying strictness
# ------------------------------------------------------------------------------
# @param Y the outcome
# @param X the features
# @param family the distribution/link function to use (e.g., "gaussian")
# @param obsWeights observation weights
# @param id vector of ids
# @param minPvalue the minimum p-value threshold to use
# @param ... other args to screen.corRank (part of the SuperLearner package)
screen.corP.05 <- function(Y, X, family, obsWeights, id, minPvalue = 0.05, ...) {
  screen.corP(Y = Y, X = X, family = family, obsWeights = obsWeights, id = id,
              minPvalue = minPvalue, ...)
}
screen.corP.20 <- function(Y, X, family, obsWeights, id, minPvalue = 0.2, ...) {
  screen.corP(Y = Y, X = X, family = family, obsWeights = obsWeights, id = id,
              minPvalue = minPvalue, ...)
}
screen.corP.40 <- function(Y, X, family, obsWeights, id, minPvalue = 0.4, ...) {
  screen.corP(Y = Y, X = X, family = family, obsWeights = obsWeights, id = id,
              minPvalue = minPvalue, ...)
}
# ------------------------------------------------------------------------------
# screen based on random forests
# ------------------------------------------------------------------------------
# @param Y the outcome
# @param X the features
# @param newX the new features
# @param family the distribution/link function to use (e.g., "gaussian")
# @param nVar the number of variables to select
# @param ntree the number of trees to use (we use the ranger default)
# @param mtry the number of variables to try at each split (we use the ranger default)
# @param probability should we do probability-scale? (defaults to TRUE if binomial, else FALSE)
# @param min.node.size the minimum number of observations in a node (we use the ranger default)
# @param replace should we sample with replacement?
# @param sample.fraction the proportion of observations to sample
# @param num.threads for parallelization (don't use this)
# @param verbose print i/o (don't do this)
# @param ... other arguments to ranger
screen.ranger <- function(Y, X, family, nVar = 10, ntree = 500,
                          mtry = floor(sqrt(ncol(X))), probability = family$family == "binomial",
                          min.node.size = ifelse(family$family == "gaussian", 5, 1),
                          replace = TRUE, sample.fraction = ifelse(replace, 1, 0.632),
                          num.threads = 1, verbose = FALSE, ...) {
  rf_fit <- SL.ranger.imp(Y = Y, X = X, newX = X, family = family, ntree = ntree, mtry = mtry,
                          probability = probability, min.node.size = min.node.size,
                          replace = replace, sample.fraction = sample.fraction,
                          num.threads = num.threads, verbose = verbose, ...)
  rf_import <- ranger::importance(rf_fit$fit$object)
  whichVariable <- rank(-abs(rf_import)) <= nVar
  whichVariable
}
screen.ranger.4 <- function(Y, X, family, nVar = 4, ...) {
  screen.ranger(Y = Y, X = X, family = family, nVar = nVar, ...)
}
screen.ranger.10 <- function(Y, X, family, nVar = 10, ...) {
  screen.ranger(Y = Y, X = X, family = family, nVar = nVar, ...)
}
screen.ranger.25 <- function(Y, X, family, nVar = 25, ...) {
  screen.ranger(Y = Y, X = X, family = family, nVar = nVar, ...)
}
