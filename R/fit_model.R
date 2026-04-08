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
#' @param spatial_control Logical. If \code{TRUE} (default), the spatial control
#'   is included in the model. If \code{FALSE}, the spatial control is not
#'   included.
#' @param adapt_delta Target acceptance probability for the NUTS sampler.
#' @param iter_warmup Number of warmup iterations.
#' @param iter_sampling Number of sampling iterations.
#' @param chains Number of Markov chains to run.
#' @param parallel_chains Number of Markov chains to run in parallel.
#'
#' @returns coevfit object
#'
fit_model <- function(data, tree, model, prior_only = FALSE,
                      spatial_control = TRUE, adapt_delta = 0.99,
                      iter_warmup = 1000, iter_sampling = 1000,
                      chains = 4, parallel_chains = 4) {
  # set temporary options
  withr::with_options(list(cmdstanr_warn_inits = FALSE), {
    # ensure binary variables are 0/1 integers in data for coevolve
    variables <- get_variables_list(model)
    for (j in 1:length(variables)) {
      if (variables[[j]] == "bernoulli_logit") {
        variable_name <- names(variables)[j]
        data[[variable_name]] <- as.integer(data[[variable_name]] == "Present")
      }
    }
    # get tree as multiphylo object
    tree <- ape::keep.tip.multiPhylo(
      phytools::as.multiPhylo(tree),
      tip = data$xd_id
    )
    # get priors for model
    priors <- list(
      b          = "std_normal()",
      A_diag     = "double_exponential(0, 0.5)",
      A_offdiag  = "double_exponential(0, 2)",
      Q_sigma    = "exponential(2)",
      eta_anc    = "std_normal()",
      c          = "normal(0, 3)",
      sigma_dist = "exponential(2)",
      rho_dist   = "exponential(6)"
    )
    # get effects matrix
    effects_matrix <- get_effects_matrix(model)
    # get number of traits
    j <- nrow(effects_matrix)
    # function to get initial values for mcmc
    initialise <- function() {
      list(
        A_diag = rep(-0.5, j),
        A_offdiag = rep(0, sum(effects_matrix) - j),
        Q_sigma = rep(0.5, j)
      )
    }
    # fit model in cmdstanr
    coev_fit(
      data = data,
      variables = variables,
      id = "xd_id",
      tree = tree,
      effects_mat = effects_matrix,
      lon_lat = if (spatial_control) {
        dplyr::rename(data, id = xd_id)
      } else {
        NULL
      },
      dist_k = 20,
      estimate_correlated_drift = FALSE,
      prior = priors,
      prior_only = prior_only,
      adapt_delta = adapt_delta,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      chains = chains,
      parallel_chains = parallel_chains,
      init = initialise,
      seed = 1234
    )
  })
}
