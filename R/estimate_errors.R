#' @title Estimate record linkage errors
#'
#' @export
estimate_errors <- function(x = NULL,
                            y = NULL,
                            blocking_result = NULL,
                            n = NULL,
                            N = NULL,
                            G,
                            alpha = NULL,
                            p = NULL,
                            lambda = NULL,
                            tol = 10^(-10),
                            maxiter = 1000) {

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

  convergent <- FALSE
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
    print(l)
    if (l >= 2) {
      log_lik_old <- log_lik_new
      log_lik_new <- sum(log(probs_n_c %*% as.matrix(alpha)))
    } else {
      log_lik_new <- sum(log(probs_n_c %*% as.matrix(alpha)))
      next
    }

    if (abs(log_lik_new - log_lik_old) <= tol) {
      convergent <- TRUE
      break
    }

  }

  FNR <- 1 - sum(alpha * p)
  FPR <- sum(alpha * lambda) / (N - 1)

  return(list(
    FPR = FPR,
    FNR = FNR,
    convergent = convergent
  ))

}
