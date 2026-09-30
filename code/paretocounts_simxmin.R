###############################################################################
# Modified paretocounts_simxmin family for brms
#
# Changes:
#   * xmin is now an estimable distributional parameter
#   * xmax remains data supplied via vreal()
#   * counts remains data supplied via vreal()
#
# Example formula:
#
# bf(
#   dw_mg | vreal(counts, xmax) ~ 1,
#   xmin ~ 1
# )
#
###############################################################################

utils::globalVariables(c("x", "vreal2"))

###############################################################################
# Random generation
###############################################################################

rparetocounts_simxmin <- function(n = 300,
                          lambda = -1.2,
                          xmin = 1,
                          xmax = 1000) {
  
  samples <- numeric(n)
  
  if (any(xmin <= 0 | xmin >= xmax)) {
    warning("Parameters out of bounds in rparetocounts_simxmin. Returning NA.")
    return(rep(NA, n))
  }
  
  u <- stats::runif(n)
  
  ifelse(
    lambda != -1,
    (u * xmax^(lambda + 1) +
       (1 - u) * xmin^(lambda + 1))^(1 / (lambda + 1)),
    xmax^u * xmin^(1 - u)
  )
}

###############################################################################
# Density
###############################################################################

dparetocounts_simxmin <- function(x, lambda, xmin, xmax) {
  
  if (xmin <= 0 || xmin >= xmax) {
    stop("Parameters out of bounds in dparetocounts_simxmin")
  }
  
  if (x < xmin || x > xmax) {
    return(0)
  }
  
  if (lambda != -1) {
    
    density <-
      (lambda + 1) *
      (x^(lambda + 1)) /
      (xmax^(lambda + 1) - xmin^(lambda + 1))
    
  } else {
    
    density <-
      x^(-2) /
      (xmin * log(xmax / xmin))
    
  }
  
  density
}

###############################################################################
# CDF
###############################################################################

pparetocounts_simxmin <- function(x, xmin, xmax, lambda) {
  
  if (xmin <= 0 || xmin >= xmax) {
    stop("Parameters out of bounds. Is xmin negative?")
  }
  
  if (x < xmin || x > xmax) {
    return(0)
  }
  
  if (lambda == -1) {
    
    return(
      1 -
        (log(x) - log(xmin)) /
        (log(xmax) - log(xmin))
    )
    
  } else {
    
    return(
      1 -
        (
          (x^(lambda + 1) - xmin^(lambda + 1)) /
            (xmax^(lambda + 1) - xmin^(lambda + 1))
        )
    )
  }
}

###############################################################################
# R-side log likelihood
###############################################################################

paretocounts_simxmin_lpdf_temp <- function(
    Y,
    mu,
    xmin,
    counts,
    xmax
) {
  
  if (xmin <= 0 || xmin >= xmax)
    return(-Inf)
  
  if (mu != -1) {
    
    return(
      counts *
        (
          log(
            (mu + 1) /
              (xmax^(mu + 1) - xmin^(mu + 1))
          ) +
            mu * log(Y)
        )
    )
  }
  
  counts *
    -(
      log(log(xmax) - log(xmin)) +
        mu * log(Y)
    )
}

paretocounts_simxmin_lpdf <- Vectorize(paretocounts_simxmin_lpdf_temp)

###############################################################################
# brms log-likelihood method
###############################################################################

log_lik_paretocounts_simxmin <- function(i, prep) {
  
  mu <- brms::get_dpar(prep, "mu", i = i)
  
  xmin <- brms::get_dpar(prep, "xmin", i = i)
  
  counts <- prep$data$vreal1[i]
  
  xmax <- prep$data$vreal2[i]
  
  Y <- prep$data$Y[i]
  
  paretocounts_simxmin_lpdf(
    Y = Y,
    mu = mu,
    xmin = xmin,
    counts = counts,
    xmax = xmax
  )
}

###############################################################################
# posterior_predict
###############################################################################

posterior_predict_paretocounts_simxmin <- function(i, prep, ...) {
  
  mu <- brms::get_dpar(prep, "mu", i = i)
  
  xmin <- brms::get_dpar(prep, "xmin", i = i)
  
  xmax <- prep$data$vreal2[i]
  
  rparetocounts_simxmin(
    prep$ndraws,
    lambda = mu,
    xmin = xmin,
    xmax = xmax
  )
}

###############################################################################
# posterior_epred
###############################################################################

posterior_epred_paretocounts_simxmin <- function(prep) {
  
  mu <- prep$dpars$mu
  
  return(mu)
}

###############################################################################
# Stan likelihood
###############################################################################

stan_funs <- "
real paretocounts_simxmin_lpdf(
  real Y,
  real mu,
  real xmin,
  real counts,
  real xmax
){

  if (xmin <= 0 || xmin >= xmax)
    return negative_infinity();

  if(mu != -1){

    return(
      counts *
      (
        log(
          (mu + 1) /
          (pow(xmax, mu + 1) - pow(xmin, mu + 1))
        )
        +
        mu * log(Y)
      )
    );

  } else {

    return(
      counts *
      -(
        log(log(xmax) - log(xmin))
        +
        mu * log(Y)
      )
    );

  }
}
"

stanvars <- brms::stanvar(
  scode = stan_funs,
  block = "functions"
)

###############################################################################
# Custom family definition
###############################################################################

paretocounts_simxmin <- function() {
  
  brms::custom_family(
    "paretocounts_simxmin",
    
    dpars = c(
      "mu",
      "xmin"
    ),
    
    links = c(
      "identity",
      "log"
    ),
    
    lb = c(
      -Inf,
      0
    ),
    
    ub = c(
      Inf,
      Inf
    ),
    
    type = "real",
    
    vars = c(
      "vreal1[n]",  # counts
      "vreal2[n]"   # xmax
    )
  )
}

###############################################################################
# Example model
###############################################################################

# fit <- brm(
#   bf(
#     dw_mg | vreal(counts, xmax) ~ 1,
#     xmin ~ 1
#   ),
#   data = dat,
#   family = paretocounts_simxmin(),
#   stanvars = stanvars
# )

###############################################################################
# Example with predictors on xmin
###############################################################################

# fit <- brm(
#   bf(
#     dw_mg | vreal(counts, xmax) ~ treatment,
#     xmin ~ treatment
#   ),
#   data = dat,
#   family = paretocounts_simxmin(),
#   stanvars = stanvars
# )