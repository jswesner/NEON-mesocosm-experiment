// generated with brms 2.17.0
functions {
  
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  // data for splines
  int Ks; // number of linear effects
  matrix[N, Ks] Xs; // design matrix for the linear effects
  // data for spline s(run_hr, bs = "tp")
  int nb_1; // number of bases
  array[nb_1] int knots_1; // number of knots
  // basis function matrices
  matrix[N, knots_1[1]] Zs_1_1;
  int prior_only; // should the likelihood be ignored?
}
transformed data {
  
}
parameters {
  real Intercept; // temporary intercept for centered predictors
  vector[Ks] bs; // spline coefficients
  // parameters for spline s(run_hr, bs = "tp")
  // standarized spline coefficients
  vector[knots_1[1]] zs_1_1;
  real<lower=0> sds_1_1; // standard deviations of spline coefficients
  real<lower=0> sigma; // dispersion parameter
}
transformed parameters {
  // actual spline coefficients
  vector[knots_1[1]] s_1_1;
  real lprior = 0; // prior contributions to the log posterior
  // compute actual spline coefficients
  s_1_1 = sds_1_1 * zs_1_1;
  lprior += student_t_lpdf(Intercept | 3, 8.6, 3.9);
  lprior += student_t_lpdf(sds_1_1 | 3, 0, 3.9)
            - 1 * student_t_lccdf(0 | 3, 0, 3.9);
  lprior += student_t_lpdf(sigma | 3, 0, 3.9)
            - 1 * student_t_lccdf(0 | 3, 0, 3.9);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + rep_vector(0.0, N) + Xs * bs + Zs_1_1 * s_1_1;
    target += normal_lpdf(Y | mu, sigma);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(zs_1_1);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
  // additionally sample draws from priors
  real prior_Intercept = student_t_rng(3, 8.6, 3.9);
  real prior_sds_1_1 = student_t_rng(3, 0, 3.9);
  real prior_sigma = student_t_rng(3, 0, 3.9);
  // use rejection sampling for truncated priors
  while (prior_sds_1_1 < 0) {
    prior_sds_1_1 = student_t_rng(3, 0, 3.9);
  }
  while (prior_sigma < 0) {
    prior_sigma = student_t_rng(3, 0, 3.9);
  }
}