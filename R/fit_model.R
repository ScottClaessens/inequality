#' Fit generalised dynamic phylogenetic model
#'
#' Fit generalised dynamic phylogenetic model for a particular causal model with
#' the coevolve package. The \code{coevolve::coev_fit()} function writes the
#' Stan code, constructs the data list, and fits the model with the cmdstanr
#' package.
#'
#' @param data Tibble of input data
#' @param tree Phylogenetic tree(s)
#' @param model String of length one. Causal model to structure the effects
#'   matrix for the statistical model. Must be equal to one of the following:
#'   "agriculture", "intergenerational_wealth_transmission", "family",
#'   "population_size", "land_limited", "scalar_stress", "intergroup_conflict",
#'   "bridewealth", "craft_specialisation", or "food_storage"
#' @param prior_only Logical. If \code{FALSE} (default), the model is fitted to
#'   the data and returns a posterior distribution. If \code{TRUE}, the model
#'   samples from the prior only, ignoring the likelihood.
#' @param spatial_control Logical. If \code{TRUE} (default), the model controls
#'   for spatial autocorrelation by including Gaussian processes over geographic
#'   distances. If \code{FALSE}, the model does not include this control.
#'
#' @returns coevfit object
#'
fit_model <- function(data, tree, model, prior_only = FALSE,
                      spatial_control = TRUE) {
  # ensure binary variables are 0/1 integers in data for coevolve
  variables <- get_variables_list(model)
  for (j in 1:length(variables)) {
    if (variables[[j]] == "bernoulli_logit") {
      variable_name <- names(variables)[j]
      data[[variable_name]] <- as.integer(data[[variable_name]] == "Present")
    }
  }
  # priors for model
  priors <- list(
    b          = "std_normal()",
    A_diag     = "double_exponential(0, 1)",
    A_offdiag  = "double_exponential(0, 2)",
    Q_sigma    = "exponential(1)",
    eta_anc    = "std_normal()",
    c          = "normal(0, 3)",
    sigma_dist = "exponential(2)",
    rho_dist   = "exponential(6)"
  )
  # fit model in cmdstanr
  coev_fit(
    data = data,
    variables = variables,
    id = "xd_id",
    tree = keep.tip(tree, data$xd_id),
    effects_mat = get_effects_matrix(model),
    lon_lat = if (spatial_control) {
      transmute(data, id = xd_id, longitude, latitude)
    } else {
      NULL
    },
    dist_k = 5,
    estimate_correlated_drift = FALSE,
    prior = priors,
    prior_only = prior_only,
    adapt_delta = 0.999,
    max_treedepth = 12,
    init = 0,
    parallel_chains = 4,
    seed = 1234
  )
}
