# Estimate errors due to blocking in record linkage

Function computes estimators for false positive rate (FPR) and false
negative rate (FNR) due to blocking in record linkage, as proposed by
Dasylva and Goussanou (2021). Assumes duplicate-free data sources,
complete coverage of the reference data set and blocking decisions based
solely on record pairs.

## Usage

``` r
est_block_error(
  x = NULL,
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
  sample_size = NULL
)
```

## Arguments

- x:

  Reference data (required if `n` and `N` are not provided).

- y:

  Query data (required if `n` is not provided).

- blocking_result:

  `data.frame` or `data.table` containing blocking results (required if
  `n` is not provided). It must contain a column named `y` storing the
  indices of the records in the query data set.

- n:

  Integer vector of numbers of accepted pairs formed by each record in
  the query data set with records in the reference data set, based on
  blocking criteria (if `NULL`, derived from `blocking_result`).

- N:

  Total number of records in the reference data set (if `NULL`, derived
  as `length(x)`).

- G:

  Integer or vector of integers. Number of classes in the finite mixture
  model. If `G` is a vector, the optimal number of classes is selected
  from the provided values based on the Akaike Information Criterion
  (AIC).

- alpha:

  Numeric vector of initial class proportions (length `G`; if `NULL`,
  initialized as `rep(1/G, G)`).

- p:

  Numeric vector of initial matching probabilities in each class of the
  mixture model (length `G`; if `NULL`, randomly initialized from
  `runif(G, 0.5, 1)` or `rep(runif(1, 0.5, 1), G)`, depending on the
  parameter `equal_p`).

- lambda:

  Numeric vector of initial Poisson distribution parameters for
  non-matching records in each class of the mixture model (length `G`;
  if `NULL`, randomly initialized from `runif(G, 0.1, 2)`).

- equal_p:

  Logical, indicating whether the matching probabilities `p` should be
  constrained to be equal across all latent classes (default `FALSE`).

- tol:

  Convergence tolerance for the EM algorithm (default `10^(-4)`).

- maxiter:

  Maximum number of iterations for the EM algorithm (default `100`).

- sample_size:

  Bootstrap sample (from `n`) size used for calculations (if `NULL`,
  uses all data).

## Value

Returns an object of class `est_block_error`, with a list containing:  

- `FPR` – estimated false positive rate,

- `FNR` – estimated false negative rate,

- `G` – number of classes used in the optimal model,

- `log_lik` – final log-likelihood value,

- `equal_p` – logical, indicating whether the matching probabilities
  were constrained,

- `iter` – number of the EM algorithm iterations performed,

- `convergence` – logical, indicating whether the EM algorithm converged
  within `maxiter` iterations,

- `AIC` – Akaike Information Criterion value in the optimal model.

## Details

Consider a large finite population that comprises of \\N\\ individuals,
and two duplicate-free data sources: a register (reference data `x`) and
a file (query data `y`). Assume that the register has no undercoverage,
i.e., each record from the file corresponds to exactly one record from
the same individual in the register. Let \\n_i\\ denote the number of
register records which form an accepted (by the blocking criteria) pair
with record \\i\\ on the file, for \\i=1,2,\ldots,m\\, where \\m\\ is
the number of records in the file. Let \\v_i\\ denote record \\i\\ from
the file. Assume that:  

- two matched records are neighbours with a probability that is bounded
  away from \\0\\ regardless of \\N\\,

- two unmatched records are accidental neighbours with a probability of
  \\O(\frac{1}{N})\\.

The finite mixture model \\n_i \sim \sum\_{g=1}^G
\alpha_g(\text{Bernoulli}(p_g) \ast \text{Poisson}(\lambda_g))\\ is
assumed. When \\G\\ is fixed, the unknown model parameters are given by
the vector \\\psi = \[(\alpha_g, p_g, \lambda_g)\]\_{1 \leq g \leq G}\\
that may be estimated with the Expectation-Maximization (EM) procedure.

Let \\n_i = n\_{i\|M} + n\_{i\|U}\\, where \\n\_{i\|M}\\ is the number
of matched neighbours and \\n\_{i\|U}\\ is the number of unmatched
neighbours, and let \\c\_{ig}\\ denote the indicator that record \\i\\
is from class \\g\\. For the E-step of the EM procedure, the equations
are as follows \$\$ \begin{aligned} P(n_i \| c\_{ig} = 1) &= I(n_i =
0)(1-p_g)e^{-\lambda_g}+I(n_i \>
0)\Bigl(p_g+(1-p_g)\frac{\lambda_g}{n_i}\Bigr)\frac{e^{-\lambda_g}\lambda_g^{n_i-1}}{(n_i-1)!},
\\ P(c\_{ig} = 1 \| n_i) &= \frac{\alpha_gP(n_i \| c\_{ig} =
1)}{\sum\_{g'=1}^G\alpha\_{g'}P(n_i \| c\_{ig'} = 1)}, \\ P(n\_{i\|M} =
1 \| n_i,c\_{ig} = 1) &= \frac{p_gn_i}{p_gn_i + (1-p_g)\lambda_g}, \\
P(n\_{i\|U} = n_i \| n_i,c\_{ig} = 1) &= I(n_i = 0) + I(n_i \>
0)\frac{(1-p_g)\lambda_g}{p_gn_i + (1-p_g)\lambda_g}, \\ P(n\_{i\|U} =
n_i-1 \| n_i,c\_{ig} = 1) &= \frac{p_gn_i}{p_gn_i + (1-p_g)\lambda_g},
\\ E\[c\_{ig}n\_{i\|M} \| n_i\] &= P(c\_{ig} = 1 \| n_i)P(n\_{i\|M} = 1
\| n_i,c\_{ig} = 1), \\ E\[n\_{i\|U} \| n_i,c\_{ig} = 1\] &=
\Bigl(\frac{p_g(n_i-1) + (1-p_g)\lambda_g}{p_gn_i +
(1-p_g)\lambda_g}\Bigr)n_i, \\ E\[c\_{ig}n\_{i\|U} \| n_i\] &= P(c\_{ig}
= 1 \| n_i)E\[n\_{i\|U} \| n_i,c\_{ig} = 1\]. \end{aligned} \$\$ The
M-step is given by following equations \$\$ \begin{aligned} \hat{p}\_g
&= \frac{\sum\_{i=1}^mE\[c\_{ig}n\_{i\|M} \|
n_i;\psi\]}{\sum\_{i=1}^mE\[c\_{ig} \| n_i; \psi\]}, \\ \hat{\lambda}\_g
&= \frac{\sum\_{i=1}^mE\[c\_{ig}n\_{i\|U} \| n_i;
\psi\]}{\sum\_{i=1}^mE\[c\_{ig} \| n_i; \psi\]}, \\ \hat{\alpha}\_g &=
\frac{1}{m}\sum\_{i=1}^mE\[c\_{ig} \| n_i; \psi\]. \end{aligned} \$\$ As
\\N \to \infty\\, the error rates and the model parameters are related
as follows \$\$ \begin{aligned} \text{FNR} &\xrightarrow{p} 1 -
E\[p(v_i)\], \\ (N-1)\text{FPR} &\xrightarrow{p} E\[\lambda(v_i)\],
\end{aligned} \$\$ where \\E\[p(v_i)\] = \sum\_{g=1}^G\alpha_gp_g\\ and
\\E\[\lambda(v_i)\] = \sum\_{g=1}^G\alpha_g\lambda_g\\.

## Note

The matching probabilities \\p_g\\ can be constrained to be equal across
all latent classes by setting `equal_p = TRUE`.

## References

Dasylva, A., Goussanou, A. (2021). Estimating the false negatives due to
blocking in record linkage. Survey Methodology, Statistics Canada,
Catalogue No. 12-001-X, Vol. 47, No. 2.

Dasylva, A., Goussanou, A. (2022). On the consistent estimation of
linkage errors without training data. Jpn J Stat Data Sci 5, 181–216.
[doi:10.1007/s42081-022-00153-3](https://doi.org/10.1007/s42081-022-00153-3)

## Examples

``` r
## an example proposed by Dasylva and Goussanou (2021)
## we obtain results very close to those reported in the paper

set.seed(11)

neighbors <- rep(0:5, c(1659, 53951, 6875, 603, 62, 5))

errors <- est_block_error(n = neighbors,
                          N = 63155,
                          G = 1:3,
                          tol = 10^(-3),
                          equal_p = TRUE)

errors
#> Estimated FPR:  2.13617e-06 
#> Estimated FNR:  0.02994969 
#> Number of classes in the model:  2 
#> ========================================================
#> EM algorithm converged successfully within 92 iterations.
```
