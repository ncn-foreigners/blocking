#' @importFrom stats dpois
#' @importFrom stats runif
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
#' @param n Integer vector of numbers of accepted pairs formed by each record in the query data set
#' with records in the reference data set, based on blocking criteria (if `NULL`, derived from `blocking_result`).
#' @param N Total number of records in the reference data set (if `NULL`, derived as `length(x)`).
#' @param G Number of classes in the finite mixture model.
#' @param alpha Numeric vector of initial class proportions (length `G`; if `NULL`, initialized as `rep(1/G, G)`).
#' @param p Numeric vector of initial matching probabilities in each class of the mixture model
#' (length `G`; if `NULL`, randomly initialized from `runif(G, 0.5, 1)`).
#' @param lambda Numeric vector of initial Poisson distribution parameters for non-matching records in each class of the mixture model
#' (length `G`; if `NULL`, randomly initialized from `runif(G, 0.1, 2)`).
#' @param tol Convergence tolerance for the EM algorithm (default `10^(-6)`).
#' @param maxiter Maximum number of iterations for the EM algorithm (default `1000`).
#' @param sample_size Bootstrap sample (from `n`) size used for calculations (if `NULL`, uses all data).
#'
#' @details
#' Consider a large finite population that comprises of \eqn{N} individuals, and two duplicate-free data sources: a register and a file.
#' Assume that the register has no undercoverage,
#' i.e. each record from the file corresponds to exactly one record from the same individual in the register.
#' Let \eqn{n_i} denote the number of register records which form an accepted (by the blocking criteria) pair with
#' record \eqn{i} on the file. Assume that:\cr
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
#' P(n_i | c_{ig} = 1) = I(n_i = 0)(1-p_g)e^{-\lambda_g}+I(n_i > 0)\Bigl(p_g+(1-p_g)\frac{\lambda_g}{n_i}\Bigr)\frac{e^{-\lambda_g}\lambda_g^{n_i-1}}{(n_i-1)!},
#' }
#' \deqn{
#' P(c_{ig} = 1 | n_i) = \frac{\alpha_gP(n_i | c_{ig} = 1)}{\sum_{g'=1}^G\alpha_{g'}P(n_i | c_{ig'} = 1)},
#' }
#' \deqn{
#' P(n_{i|M} = 1 | n_i,c_{ig} = 1) = \frac{p_gn_i}{p_gn_i + (1-p_g)\lambda_g},
#' }
#' \deqn{
#' P(n_{i|U} = n_i | n_i,c_{ig} = 1) = I(n_i = 0) + I(n_i > 0)\frac{(1-p_g)\lambda_g}{p_gn_i + (1-p_g)\lambda_g},
#' }
#' \deqn{
#' P(n_{i|U} = n_i-1 | n_i,c_{ig} = 1) = \frac{p_gn_i}{p_gn_i + (1-p_g)\lambda_g},
#' }
#' \deqn{
#' E[c_{ig}n_{i|M} | n_i] = P(c_{ig} = 1 | n_i)P(n_{i|M} = 1 | n_i,c_{ig} = 1),
#' }
#' \deqn{
#' E[n_{i|U} | n_i,c_{ig} = 1] = \Bigl(\frac{p_g(n_i-1) + (1-p_g)\lambda_g}{p_gn_i + (1-p_g)\lambda_g}\Bigr)n_i,
#' }
#' \deqn{
#' E[c_{ig}n_{i|U} | n_i] = P(c_{ig} = 1 | n_i)E[n_{i|U} | n_i,c_{ig} = 1].
#' }
#' The M-step is given by following equations
#' \deqn{
#' \hat{p}_g = \frac{\sum_{i=1}^mE[c_{ig}n_{i|M} | n_i;\psi]}{\sum_{i=1}^mE[c_{ig} | n_i; \psi]},
#' }
#' \deqn{
#' \hat{\lambda}_g = \frac{\sum_{i=1}^mE[c_{ig}n_{i|U} | n_i; \psi]}{\sum_{i=1}^mE[c_{ig} | n_i; \psi]},
#' }
#' \deqn{
#' \hat{\alpha}_g = \frac{1}{m}\sum_{i=1}^mE[c_{ig} | n_i; \psi].
#' }
#'
#' As \eqn{N \to \infty}, the error rates and the model parameters are related as follows
#' \deqn{
#' \text{FNR} \xrightarrow{p} 1 - E[p(v_i)],
#' }
#' \deqn{
#' (N-1)\text{FPR} \xrightarrow{p} E[\lambda(v_i)],
#' }
#' where \eqn{E[p(v_i)] = \sum_{g=1}^G\alpha_gp_g} and \eqn{E[\lambda(v_i)] = \sum_{g=1}^G\alpha_g\lambda_g}.
#'
#'
#'
#' @returns Returns a list containing:\cr
#' \itemize{
#' \item{`FPR` -- estimated false positive rate,}
#' \item{`FNR` -- estimated false negative rate,}
#' \item{`iter` -- number of the EM algorithm iterations performed,}
#' \item{`convergence` -- logical, indicating whether the EM algorithm converged within `maxiter` iterations.}
#' }
#'
#' @references
#' Dasylva, A., Goussanou, A. (2021). Estimating the false negatives due to blocking in record linkage.
#' Survey Methodology, Statistics Canada, Catalogue No. 12-001-X, Vol. 47, No. 2.
#'
#' Dasylva, A., Goussanou, A. (2022). On the consistent estimation of linkage errors without training data.
#' Jpn J Stat Data Sci 5, 181–216. \url{https://doi.org/10.1007/s42081-022-00153-3}
#'
#' @examples
#' ## an example proposed by Dasylva and Goussanou (2021)
#'
#' set.seed(111)
#'
#' neighbors <- rep(0:5, c(1659, 53951, 6875, 603, 62, 5))
#'
#' errors <- est_block_error(n = neighbors,
#'                           N = 63155,
#'                           G = 2,
#'                           tol = 10^(-3),
#'                           maxiter = 50)
#'
#' errors
#'
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
                            tol = 10^(-6),
                            maxiter = 1000,
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

  convergence <- FALSE
  m <- length(n)

  if (is.null(alpha)) {
    alpha <- rep(1/G, G)
  }

  if (is.null(p)) {
    p <- runif(G, min = 0.5, max = 1)
  }

  if (is.null(lambda)) {
    lambda <- runif(G, min = 0.1, max = 2)
  }

  for (l in 1:maxiter) {

    ## E

    # probs_n_c <- matrix(0, m, G)
    #
    # for (i in 1:m) {
    #   for (g in 1:G) {
    #     if (n[i] == 0) {
    #       probs_n_c[i, g] <- (1 - p[g]) * exp(-lambda[g])
    #     } else {
    #       probs_n_c[i, g] <- (p[g] + (1 - p[g]) * lambda[g] / n[i]) *
    #         exp(-lambda[g]) * lambda[g]^(n[i] - 1) / factorial(n[i] - 1)
    #     }
    #   }
    # }

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

    # probs_c_n <- matrix(0, m, G)
    #
    # for (i in 1:m) {
    #   for (g in 1:G) {
    #     probs_c_n[i, g] <-
    #       alpha[g] * probs_n_c[i, g] / sum(alpha * as.vector(probs_n_c[i, ]))
    #   }
    # }

    # probs_c_n <- t(
    #   t(probs_n_c * alpha) * as.vector(1 / (probs_n_c %*% alpha))
    # )

    probs_c_n <- probs_n_c * alpha / as.vector(probs_n_c %*% alpha)

    # probs_n_M <- matrix(0, m, G)
    #
    # for (i in 1:m) {
    #   for (g in 1:G) {
    #     probs_n_M[i, g] <-
    #       p[g] * n[i] / (p[g] * n[i] + (1 - p[g]) * lambda[g])
    #   }
    # }

    # probs_n_M <- mapply(
    #   function(x, y) {
    #     x * n /  (x * n + (1 - x) * y)
    #   },
    #   p, lambda,
    #   SIMPLIFY = TRUE
    # )

    n_mat <- matrix(n, nrow = m, ncol = G)
    p_mat <- matrix(p, nrow = m, ncol = G, byrow = TRUE)
    lambda_mat <- matrix(lambda, nrow = m, ncol = G, byrow = TRUE)

    probs_n_M <- n_mat * p_mat / (n_mat * p_mat + (1 - p_mat) * lambda_mat)

    # probs_n_U <- matrix(0, m, G)
    #
    # for (i in 1:m) {
    #   for (g in 1:G) {
    #     if (n[i] == 0) {
    #       probs_n_U[i, g] <- 1
    #     } else {
    #       probs_n_U[i, g] <- (1 - p[g]) * lambda[g] / (p[g] * n[i] + (1 - p[g]) * lambda[g])
    #     }
    #   }
    # }

    probs_n_U <- mapply(
      function(x, y) {
        ifelse(n == 0,
               1,
               (1 - x) * y / (x * n + (1 - x) * y))
      },
      p, lambda,
      SIMPLIFY = TRUE
    )

    # E_c_n_M <- matrix(0, m, G)
    #
    # for (i in 1:m) {
    #   for (g in 1:G) {
    #     E_c_n_M[i, g] <-  probs_c_n[i, g] * probs_n_M[i, g]
    #   }
    # }

    E_c_n_M <- probs_c_n * probs_n_M

    # E_n_U <- matrix(0, m, G)
    #
    # for (i in 1:m) {
    #   for (g in 1:G) {
    #     E_n_U[i, g] <-
    #       ((p[g] * (n[i] - 1) + (1 - p[g]) * lambda[g]) / (p[g] * n[i] + (1 - p[g]) * lambda[g])) * n[i]
    #   }
    # }

    E_n_U<- ((p_mat * (n_mat - 1) + (1 - p_mat) * lambda_mat) / (p_mat * n_mat + (1 - p_mat) * lambda_mat)) * n_mat

    # E_c_n_U <- matrix(0, m, G)
    #
    # for (i in 1:m) {
    #   for (g in 1:G) {
    #     E_c_n_U[i, g] <- probs_c_n[i, g] * E_n_U[i, g]
    #   }
    # }

    E_c_n_U <- probs_c_n * E_n_U

    ## M

    alpha <- 1 / m * colSums(probs_c_n)
    p <- colSums(E_c_n_M) / (m * alpha)
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

  return(structure(
    list(
    FPR = FPR,
    FNR = FNR,
    iter = l,
    convergence = convergence
  ),
  class = "est_block_error"))

}
