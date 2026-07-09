data {
  int<lower=0> N;                             // number of observed data points
  int<lower=1> P;                             // number of phylogenetic trees
  int<lower=2> K;                             // number of ordinal levels
  array[N] int y;                             // observed outcome variable
  array[P] cholesky_factor_cov[N] L_cor_phy;  // cholesky of phy cor matrix
  cholesky_factor_cov[N] L_cor_geo;           // cholesky of geo cor matrix
}
parameters {
  ordered[K - 1] cutpoints; // ordinal cutpoints
  real<lower=0> sigma_phy;  // phylogenetic variance
  real<lower=0> sigma_geo;  // geographic variance
  vector[N] z_phy;          // phylogenetic random intercepts (std.)
  vector[N] z_geo;          // geographic random intercepts (std.)
}
model {
  // initialise vectors
  vector[N] phy_effect;
  vector[N] geo_effect;
  vector[P] lp = rep_vector(0.0, P);

  // priors
  cutpoints ~ normal(0, 2);
  z_phy ~ normal(0, 1);
  z_geo ~ normal(0, 1);
  sigma_phy ~ normal(0, 2);
  sigma_geo ~ normal(0, 2);

  // likelihood
  geo_effect = (sigma_geo * (L_cor_geo * z_geo));
  for (p in 1:P) {
    phy_effect = (sigma_phy * (L_cor_phy[p] * z_phy));
    lp[p] += ordered_logistic_lpmf(y | phy_effect + geo_effect, cutpoints);
  }

  // average over trees
  target += log_sum_exp(lp) - log(P);
}
