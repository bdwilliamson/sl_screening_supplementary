## generate data for missing data feature selection simulation

expit <- function(x) exp(x) / (1 + exp(x))
## generate X data
#' @param n the sample size
#' @param p the number of covariates
#' @param rho the correlation
#' @param rho2 the correlation within the active set
#' @param x_dist the distribution of x
#' @param active_set the active set
#' @return matrix of covariates
gen_x <- function(n, p, rho = 0, rho2 = 0, x_dist = "normal", corr_type = "simple", active_set = 1:6) {
  if (x_dist == "normal") {
    mat <- diag(p)
    sig <- rho ^ abs(row(mat) - col(mat))
    if (corr_type == "complex") {
      # sig2 <- (-1/2) * rho ^ abs(row(mat2) - col(mat2))
      sig2 <- matrix(rho2, nrow = length(active_set), ncol = length(active_set))
      diag(sig2) <- 1
      sig[1:length(active_set), 1:length(active_set)] <- sig2
    }
    # if (corr_type == "complex") {
    #   sigs <- rep(list(matrix(c(1, rho, rho, 1), byrow = TRUE, nrow = 2)), p / 2)
    #   sig <- Matrix::bdiag(sigs)
    # }
    x <- MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = sig)
    if (corr_type == "complex") {
      # permute the columns
      x2 <- x
      x2[, active_set] <- x[, 1:length(active_set)]
      x2[, -active_set] <- x[, (length(active_set) + 1):p]
      x <- x2
    }
  } else if (x_dist == "nonnormal") {
    x1 <- rnorm(n, mean = 0.5, sd = 1)
    x2 <- rbinom(n, size = 1, prob = 0.5)
    x3 <- rweibull(n, shape = 1.75, scale = 1.9)
    log_x4 <- rnorm(n, mean = 0.5, sd = 0.5)
    x4 <- exp(log_x4)
    x5 <- rbinom(n, size = 1, prob = 0.5)
    x6 <- rnorm(n, mean = 0.25, sd = 1)
    if (p > 6) {
      mat <- diag(p - 6)
      sig <- rho ^ abs(row(mat) - col(mat))
      other_x <- MASS::mvrnorm(n = n, mu = rep(0, p - 6), Sigma = sig)
    } else {
      other_x <- NULL
    }
    x <- cbind(x1, x2, x3, x4, x5, x6, other_x)
  } else {
    stop("The entered distribution for x is not currently supported. Please enter one of 'normal' or 'nonnormal'.")
  }
  colnames(x) <- rep("", ncol(x))
  return(x)
}
## generate Y data
## @param x the covariates
## @param func the function that makes the linear predictor (e.g., function(x, beta) x%*%beta for a linear model)
## @param family specifies the link function ('gaussian' is identity link, 'binomial' is logit link)
gen_y <- function(x, func, family) {
  n <- dim(x)[1]
  linear_predictor <- func(x)
  if (family$family == "gaussian") {
    return(linear_predictor + rnorm(n, 0, 1))
  } else if (family$family == "binomial") {
    if (family$link == "logit") {
      return(rbinom(n, 1, prob = expit(linear_predictor)))  
    } else {
      return(as.numeric((linear_predictor + rnorm(n, 0, 1)) > 0))
    }
  } else {
    stop("This function currently only works for family = 'gaussian' or family = 'binomial'. Please specify one of these families.")
  }
}
## generate a full dataset
gen_data <- function(n = 100, p = 6, rho = 0, rho2 = 0, func = function(x) x %*% matrix(0, ncol(x)),
                     family = "binomial", x_dist = "normal", corr_type = "simple",
                     active_set = 1:6) {
  x <- gen_x(n = n, p = p, rho = rho, rho2 = rho2, x_dist = x_dist, corr_type = corr_type,
             active_set = active_set)
  y <- gen_y(x = x, func = func, family = family)
  tib <- tibble::tibble(data.frame(y = y), as.data.frame(x))
  tib
}
## ----------------------------------------------------------------
## generate data from a linear outcome model
## ----------------------------------------------------------------
gen_data_lm <- function(n = 100, p = 6, rho = 0, rho2 = rho2, beta = matrix(rep(0, 6)),
                        family = "binomial", x_dist = "normal", y_dist = "simple", corr_type = "simple",
                        active_set = 1:6) {
  return(gen_data(n = n, p = p, rho = rho, rho2 = rho2, func = function(x) cbind(1, x)%*%beta, family = family, x_dist = x_dist,
                  corr_type = corr_type, active_set = active_set))
}
## ----------------------------------------------------------------
## generate data from a nonlinear outcome model
## specifically, 10 features matter, and the rest don't
## sparse additive model
## ----------------------------------------------------------------
# center and scale based on population mean and variance
center_scale <- function(x, mean, sd) {
  (x - mean) / sd
}
f1 <- function(x) sin(pi / 4 * x)
f2 <- function(x, y) x * y
# f3 <- function(x) tanh(x)
f3 <- function(x) x
f4 <- function(x) cos(pi / 4 * x)
f4_interact <- function(x, y) cos(pi / 4 * (x * y))
f5 <- function(x) (x ^ 2 + 1) ^ (-1)
# f6 <- function(x) (-1) * tanh(x)
f6 <- function(x) x
nl_conditional_mean <- function(x, beta, x_dist, y_dist = "many") {
  centered_x <- apply(x, 2, function(col) center_scale(col, mean = 0, sd = 1))
  if (grepl("nonnormal", x_dist)) {
    centered_x[, 1] <- apply(x[, 1, drop = FALSE], 2, center_scale, mean = 0.5, sd = 1)
    centered_x[, 2] <- apply(x[, 2, drop = FALSE], 2, center_scale, mean = 0.5, sd = sqrt(0.5))
    centered_x[, 3] <- apply(x[, 3, drop = FALSE], 2, center_scale,
                             mean = 1.9 * gamma(1 + 1 / 1.75),
                             sd = sqrt(1.9 ^ 2 * (gamma(1 + 2 / 1.75) - gamma(1 + 1 / 1.75) ^ 2)))
    centered_x[, 4] <- apply(x[, 2, drop = FALSE], 2, center_scale,
                             mean = exp(.5 + .5 ^ 2 / 2),
                             sd = sqrt((exp(.5 ^ 2) - 1) * exp(1 + .5 ^ 2)))
    centered_x[, 5] <- apply(x[, 5, drop = FALSE], 2, center_scale, mean = 0.5, sd = sqrt(0.5))
    centered_x[, 6] <- apply(x[, 6, drop = FALSE], 2, center_scale, mean = .25, sd = 1)
  }
  new_x <- centered_x
  if (grepl("many", y_dist)) {
    new_x[, 1] <- f1(centered_x[, 1])
    new_x[, 2] <- f2(centered_x[, 2], centered_x[, 3])
    new_x[, 3] <- f3(centered_x[, 3])
    new_x[, 4] <- f4(centered_x[, 4])
    new_x[, 5] <- f2(centered_x[, 5], centered_x[, 1])
    new_x[, 6] <- f6(centered_x[, 6])
  } else {
    new_x[, 1] <- f1(centered_x[, 1])
    new_x[, 2] <- f4_interact(centered_x[, 2], centered_x[, 3])
    new_x[, 3] <- f3(centered_x[, 3])
    new_x[, 4] <- f6(centered_x[, 4])
    new_x[, 5] <- f4_interact(centered_x[, 5], centered_x[, 1])
    new_x[, 6] <- f5(centered_x[, 6])
  }
  cbind(1, as.matrix(new_x)) %*% beta
}
gen_data_nlm <- function(n = 100, p = 6, rho = 0, rho2 = rho2, beta = matrix(rep(0, 6)),
                         family = "binomial", x_dist = "normal", y_dist = "simple", corr_type = "simple",
                         active_set = 1:6) {
  return(gen_data(n = n, p = p, rho = rho, rho2 = rho2,
                  func = function(x) nl_conditional_mean(x, beta, x_dist, y_dist),
                  family = family, x_dist = x_dist, corr_type = corr_type, active_set = active_set))
}