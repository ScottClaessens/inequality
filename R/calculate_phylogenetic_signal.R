#' Calculate phylogenetic signal
#'
#' Calculate phylogenetic signal for a particular variable by fitting a Bayesian
#' phylogenetic multilevel model and extracting the results
#'
#' @param data Tibble of D-PLACE data
#' @param tree Posterior treeset of D-PLACE societies
#' @param variable Variable to use as the outcome in the model
#' @param tree_id Index of posterior phylogenetic tree sample to use
#'
#' @returns A tibble of results
#'
calculate_phylogenetic_signal <- function(data, tree, variable, tree_id) {
  # choose tree posterior sample
  tree <- tree[[tree_id]]
  # get phylogenetic covariance matrix
  cov_matrix <- vcv.phylo(tree, corr = TRUE)
  # get formula for modelling
  formula <- bf(
    paste0(variable, " ~ 1 + (1 | gr(xd_id, cov = cov_matrix))")
  )
  # get family for modelling
  if ("ordered" %in% class(data[[variable]])) {
    family <- cumulative(link = "probit")
  } else {
    family <- bernoulli(link = "probit")
  }
  # fit model
  fit <-
    brm(
      formula = formula,
      data = data,
      family = family,
      prior = c(
        prior(normal(0, 1), class = Intercept),
        prior(exponential(0.5), class = sd)
      ),
      data2 = list(cov_matrix = cov_matrix),
      control = list(adapt_delta = 0.95),
      backend = "cmdstanr",
      cores = 4,
      seed = 123
    )
  # calculate phylogenetic signal
  post <- posterior_samples(fit)
  phylogenetic_signal <- with(
    post,
    # phylo_var / (phylo_var + residual_var)
    # see: https://doi.org/10.32942/X2SS5K
    # and see: https://ayumi-495.github.io/multinomial-GLMM-tutorial/
    sd_xd_id__Intercept^2 / (sd_xd_id__Intercept^2 + 1)
  )
  # return tibble result
  tibble(
    variable = variable,
    tree_id = tree_id,
    phylogenetic_signal = list(phylogenetic_signal),
    max_rhat = max(rhat(fit), na.rm = TRUE),
    min_neff_ratio = min(neff_ratio(fit), na.rm = TRUE),
    num_divergences = sum(
      subset(nuts_params(fit), Parameter == "divergent__")$Value
    )
  )
}
