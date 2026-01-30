#' @importFrom stats dpois
#' @importFrom stats runif
#' @importFrom stats AIC
#'
#' @title Estimate errors due to blocking in record linkage
#'
#' @description
#' Function computes estimators for false positive rate (FPR) and false negative rate (FNR) due to blocking in record linkage,
#' as proposed by Dasylva and Goussanou (2021). Assumes duplicate-free data sources,
#' complete coverage of the reference data set and blocking decisions based solely on record pairs.
#'
#' @param x Reference data (required if `n` and `N` are not provided).
#' @param y Query data (required if `n` is not provided).
#' @param blocking_result `data.frame` or `data.table` containing blocking results (required if `n` is not provided).
#' It must contain a column named `y` storing the indices of the records in the query data set.
#' @param n Integer vector of numbers of accepted pairs formed by each record in the query data set
#' with records in the reference data set, based on blocking criteria (if `NULL`, derived from `blocking_result`).
#' @param N Total number of records in the reference data set (if `NULL`, derived as `length(x)`).
#' @param G Integer or vector of integers. Number of classes in the finite mixture model.
#' If `G` is a vector, the optimal number of classes is selected from the provided values
#' based on the Akaike Information Criterion (AIC).
#' @param alpha Numeric vector of initial class proportions (length `G`; if `NULL`, initialized as `rep(1/G, G)`).
#' @param p Numeric vector of initial matching probabilities in each class of the mixture model
#' (length `G`; if `NULL`, randomly initialized from `runif(G, 0.5, 1)` or `rep(runif(1, 0.5, 1), G)`,
#' depending on the parameter `equal_p`).
#' @param lambda Numeric vector of initial Poisson distribution parameters for non-matching records in each class of the mixture model
#' (length `G`; if `NULL`, randomly initialized from `runif(G, 0.1, 2)`).
#' @param equal_p Logical, indicating whether the matching probabilities
#' `p` should be constrained to be equal across all latent classes (default `FALSE`).
#' @param tol Convergence tolerance for the EM algorithm (default `10^(-4)`).
#' @param maxiter Maximum number of iterations for the EM algorithm (default `100`).
#' @param sample_size Bootstrap sample (from `n`) size used for calculations (if `NULL`, uses all data).
#'
#' @details
#' Consider a large finite population that comprises of \eqn{N} individuals, and two duplicate-free data sources:
#' a register (reference data `x`) and a file (query data `y`).
#' Assume that the register has no undercoverage,
#' i.e., each record from the file corresponds to exactly one record from the same individual in the register.
#' Let \eqn{n_i} denote the number of register records which form an accepted (by the blocking criteria) pair with
#' record \eqn{i} on the file, for \eqn{i=1,2,\ldots,m}, where \eqn{m} is the number of records in the file.
#' Let \eqn{v_i} denote record \eqn{i} from the file.
#' Assume that:\cr
#' \itemize{
#' \item two matched records are neighbours with a probability that is bounded away from \eqn{0} regardless of \eqn{N},
#' \item two unmatched records are accidental neighbours with a probability of \eqn{O(\frac{1}{N})}.
#' }
#' The finite mixture model \eqn{n_i \sim \sum_{g=1}^G \alpha_g(\text{Bernoulli}(p_g) \ast \text{Poisson}(\lambda_g))} is assumed.
#' When \eqn{G} is fixed, the unknown model parameters are given by the vector \eqn{\psi = [(\alpha_g, p_g, \lambda_g)]_{1 \leq g \leq G}}
#' that may be estimated with the Expectation-Maximization (EM) procedure.
#'
#' Let \eqn{n_i = n_{i|M} + n_{i|U}}, where \eqn{n_{i|M}} is the number of matched neighbours
#' and \eqn{n_{i|U}} is the number of unmatched neighbours, and let \eqn{c_{ig}} denote
#' the indicator that record \eqn{i} is from class \eqn{g}.
#' For the E-step of the EM procedure, the equations are as follows
#' \deqn{
#' \begin{aligned}
#' P(n_i | c_{ig} = 1) &= I(n_i = 0)(1-p_g)e^{-\lambda_g}+I(n_i > 0)\Bigl(p_g+(1-p_g)\frac{\lambda_g}{n_i}\Bigr)\frac{e^{-\lambda_g}\lambda_g^{n_i-1}}{(n_i-1)!}, \\
#' P(c_{ig} = 1 | n_i) &= \frac{\alpha_gP(n_i | c_{ig} = 1)}{\sum_{g'=1}^G\alpha_{g'}P(n_i | c_{ig'} = 1)}, \\
#' P(n_{i|M} = 1 | n_i,c_{ig} = 1) &= \frac{p_gn_i}{p_gn_i + (1-p_g)\lambda_g}, \\
#' P(n_{i|U} = n_i | n_i,c_{ig} = 1) &= I(n_i = 0) + I(n_i > 0)\frac{(1-p_g)\lambda_g}{p_gn_i + (1-p_g)\lambda_g}, \\
#' P(n_{i|U} = n_i-1 | n_i,c_{ig} = 1) &= \frac{p_gn_i}{p_gn_i + (1-p_g)\lambda_g}, \\
#' E[c_{ig}n_{i|M} | n_i] &= P(c_{ig} = 1 | n_i)P(n_{i|M} = 1 | n_i,c_{ig} = 1), \\
#' E[n_{i|U} | n_i,c_{ig} = 1] &= \Bigl(\frac{p_g(n_i-1) + (1-p_g)\lambda_g}{p_gn_i + (1-p_g)\lambda_g}\Bigr)n_i, \\
#' E[c_{ig}n_{i|U} | n_i] &= P(c_{ig} = 1 | n_i)E[n_{i|U} | n_i,c_{ig} = 1].
#' \end{aligned}
#' }
#' The M-step is given by following equations
#' \deqn{
#' \begin{aligned}
#' \hat{p}_g &= \frac{\sum_{i=1}^mE[c_{ig}n_{i|M} | n_i;\psi]}{\sum_{i=1}^mE[c_{ig} | n_i; \psi]}, \\
#' \hat{\lambda}_g &= \frac{\sum_{i=1}^mE[c_{ig}n_{i|U} | n_i; \psi]}{\sum_{i=1}^mE[c_{ig} | n_i; \psi]}, \\
#' \hat{\alpha}_g &= \frac{1}{m}\sum_{i=1}^mE[c_{ig} | n_i; \psi].
#' \end{aligned}
#' }
#' As \eqn{N \to \infty}, the error rates and the model parameters are related as follows
#' \deqn{
#' \begin{aligned}
#' \text{FNR} &\xrightarrow{p} 1 - E[p(v_i)], \\
#' (N-1)\text{FPR} &\xrightarrow{p} E[\lambda(v_i)],
#' \end{aligned}
#' }
#' where \eqn{E[p(v_i)] = \sum_{g=1}^G\alpha_gp_g} and \eqn{E[\lambda(v_i)] = \sum_{g=1}^G\alpha_g\lambda_g}.
#'
#' @note
#' The matching probabilities \eqn{p_g} can be constrained to be equal across all latent classes
#' by setting `equal_p = TRUE`.
#'
#' @returns Returns an object of class `est_block_error`, with a list containing:\cr
#' \itemize{
#' \item{`FPR` -- estimated false positive rate,}
#' \item{`FNR` -- estimated false negative rate,}
#' \item{`G` -- number of classes used in the optimal model,}
#' \item{`log_lik` -- final log-likelihood value,}
#' \item{`equal_p` -- logical, indicating whether the matching probabilities were constrained,}
#' \item{`iter` -- number of the EM algorithm iterations performed,}
#' \item{`convergence` -- logical, indicating whether the EM algorithm converged within `maxiter` iterations,}
#' \item{`AIC` -- Akaike Information Criterion value in the optimal model.}
#' }
#'
#' @references
#' Dasylva, A., Goussanou, A. (2021). Estimating the false negatives due to blocking in record linkage.
#' Survey Methodology, Statistics Canada, Catalogue No. 12-001-X, Vol. 47, No. 2.
#'
#' Dasylva, A., Goussanou, A. (2022). On the consistent estimation of linkage errors without training data.
#' Jpn J Stat Data Sci 5, 181–216. \doi{10.1007/s42081-022-00153-3}
#'
#' @examples
#' ## an example proposed by Dasylva and Goussanou (2021)
#' ## we obtain results very close to those reported in the paper
#'
#' set.seed(11)
#'
#' neighbors <- rep(0:5, c(1659, 53951, 6875, 603, 62, 5))
#'
#' errors <- est_block_error(n = neighbors,
#'                           N = 63155,
#'                           G = 1:3,
#'                           tol = 10^(-3),
#'                           equal_p = TRUE)
#'
#' errors
#'
#' ## an example with the `blocking` function output
#'
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   library(data.table)
#'
#'   data(census)
#'   data(cis)
#'   setDT(census)
#'   setDT(cis)
#'   set.seed(2024)
#'
#'   census <- census[sample(nrow(census), floor(nrow(census) / 2)), ]
#'   cis <- cis[sample(nrow(cis), floor(nrow(cis) / 2)), ]
#'
#'   census[, txt:=paste0(pername1, pername2, sex, dob_day, dob_mon, dob_year, enumcap, enumpc)]
#'   cis[, txt:=paste0(pername1, pername2, sex, dob_day, dob_mon, dob_year, enumcap, enumpc)]
#'
#'   result <- blocking(x = census$txt,
#'                      y = cis$txt)
#'
#'   est <- est_block_error(x = census$txt,
#'                          y = census$txt,
#'                          blocking_result = result$result,
#'                          G = 1:5)
#'
#'   est
#' }
#' @export
est_block_error <- function(x = NULL,
                            y = NULL,
                            blocking_result = NULL,
                            n = NULL,
                            N = NULL,
                            G,
                            alpha = NULL,
                            p = NULL,
                            lambda = NULL,
                            equal_p = FALSE,
                            tol = 10^(-4),
                            maxiter = 100,
                            sample_size = NULL) {

  if (is.null(n)) {
    stopifnot("`x` should be a vector" = is.vector(x))
    stopifnot("`y` should be a vector" = is.vector(y))
    stopifnot("`blocking_result` should be a data.frame or a data.table" = is.data.frame(blocking_result) | is.data.table(blocking_result))
    stopifnot("`blocking_result` should contain a column named `y`" = "y" %in% colnames(blocking_result))

    n <- as.integer(table(
      factor(blocking_result$y, levels = 1:length(y))
    ))

    N <- length(x)
  }

  if (is.numeric(sample_size)) {
    n <- sample(n, size = sample_size, replace = TRUE)
  }

  if (length(G) > 1) {

    G_cand <- sort(G)
    results_list <- list()
    aic_values <- numeric(length(G_cand))

    for (i in seq_along(G_cand)) {

      fit <- est_block_error(n = n, N = N, G = G_cand[i],
                             alpha = NULL, p = NULL, lambda = NULL,
                             equal_p = equal_p, tol = tol, maxiter = maxiter)
      results_list[[i]] <- fit
      aic_values[i] <- fit$AIC

    }

    best_idx <- which.min(aic_values)
    best_model <- results_list[[best_idx]]

    return(best_model)

  }

  convergence <- FALSE
  m <- length(n)

  if (is.null(alpha)) {
    alpha <- rep(1/G, G)
  }

  if (is.null(p)) {
    if (equal_p) {
      p <- rep(runif(1, min = 0.5, max = 1), G)
    } else {
      p <- runif(G, min = 0.5, max = 1)
    }
  } else if (equal_p && length(p) == G) {
    p <- rep(mean(p), G)
  }

  if (is.null(lambda)) {
    lambda <- runif(G, min = 0.1, max = 2)
  }

  for (l in 1:maxiter) {

    ## E

    probs_n_c <- mapply(
      function(x, y) {
        ifelse(
          n == 0,
          (1 - x) * exp(-y),
          (x + (1 - x) * y / n) * dpois(n - 1, y)
        )
      },
      p, lambda,
      SIMPLIFY = TRUE)

    probs_c_n <- probs_n_c * alpha / as.vector(probs_n_c %*% alpha)

    n_mat <- matrix(n, nrow = m, ncol = G)
    p_mat <- matrix(p, nrow = m, ncol = G, byrow = TRUE)
    lambda_mat <- matrix(lambda, nrow = m, ncol = G, byrow = TRUE)

    probs_n_M <- n_mat * p_mat / (n_mat * p_mat + (1 - p_mat) * lambda_mat)

    probs_n_U <- mapply(
      function(x, y) {
        ifelse(n == 0,
               1,
               (1 - x) * y / (x * n + (1 - x) * y))
      },
      p, lambda,
      SIMPLIFY = TRUE
    )

    E_c_n_M <- probs_c_n * probs_n_M

    E_n_U<- ((p_mat * (n_mat - 1) + (1 - p_mat) * lambda_mat) / (p_mat * n_mat + (1 - p_mat) * lambda_mat)) * n_mat

    E_c_n_U <- probs_c_n * E_n_U

    ## M

    alpha <- 1 / m * colSums(probs_c_n)
    if (equal_p) {
      p <- rep(sum(E_c_n_M) / m, G)
    } else {
      p <- colSums(E_c_n_M) / (m * alpha)
    }
    lambda <- colSums(E_c_n_U) / (m * alpha)

    ## check

    if (l >= 2) {
      log_lik_old <- log_lik_new
      log_lik_new <- sum(log(probs_n_c %*% as.matrix(alpha)))
    } else {
      log_lik_new <- sum(log(probs_n_c %*% as.matrix(alpha)))
      next
    }

    if (abs(log_lik_new - log_lik_old) <= tol) {
      convergence <- TRUE
      break
    }

  }

  FNR <- 1 - sum(alpha * p)
  FPR <- sum(alpha * lambda) / (N - 1)

  res <- structure(
    list(
      FPR = FPR,
      FNR = FNR,
      G = G,
      log_lik = log_lik_new,
      equal_p = equal_p,
      iter = l,
      convergence = convergence
    ),
    class = "est_block_error")

  res$AIC <- AIC(res)

  return(res)

}
