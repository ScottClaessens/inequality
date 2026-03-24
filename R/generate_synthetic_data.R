#' Generate synthetic data for causal model
#'
#' Generate synthetic data for a particular causal model. The function manually
#' fixes parameters in the Stan code produced by the coevolve package, runs the
#' model with fixed parameters, and returns the simulated values in a tibble.
#'
#' @param data Tibble of D-PLACE data
#' @param mcc_tree Maximum clade credibility tree
#' @param model String of length one. Causal model from which to generate
#'   synthetic data. Must be equal to one of the following: "agriculture",
#'   "intergenerational_wealth_transmission", "family", "population_size",
#'   "land_limited", "scalar_stress", "intergroup_conflict", "bridewealth",
#'   "craft_specialisation", or "food_storage"
#'
#' @returns Tibble of synthetic data
#'
generate_synthetic_data <- function(data, mcc_tree, model) {
  # get list of variables for model
  variables <- get_variables_list(model)
  # ensure binary variables are 0/1 integers in data
  data <-
    data |>
    mutate(
      across(
        where(function(x) is.factor(x) & !is.ordered(x)),
        function (x) as.integer(x == "Present")
      )
    )
  # generate stan data and code with relevant variables
  stan_data <-
    suppressMessages(
      coev_make_standata(
        data = data,
        variables = variables,
        id = "xd_id",
        tree = mcc_tree,
        estimate_correlated_drift = FALSE,
        prior_only = TRUE
      )
    )
  stan_code <-
    coev_make_stancode(
      data = data,
      variables = variables,
      id = "xd_id",
      tree = mcc_tree,
      estimate_correlated_drift = FALSE
    )
  # manually fix parameters in stan code
  stan_code_fix <-
    fix_parameters(
      stan_code = stan_code,
      parameters = list(
        intercept = 0,
        auto_effect = -0.5,
        cross_effect = 2,
        drift = 2,
        init = 0
      ),
      model = model
    )
  # run simulation
  sim <-
    suppressWarnings(
      cmdstan_model(
        stan_file = write_stan_file(stan_code_fix)
      )$sample(
        data = stan_data,
        chains = 1,
        refresh = 0,
        seed = 123,
        iter_warmup = 50,
        iter_sampling = 1,
        show_messages = FALSE
      )
    )
  # construct synthetic dataset by looping over variables
  draws <- as_draws_rvars(sim)
  out <- tibble(xd_id = mcc_tree$tip.label)
  for (i in 1:length(variables)) {
    # yrep contains simulated data
    yrep <- draws_of(draws$yrep)[1, 1, 1:nrow(out), i]
    var <- names(variables)[i]
    if (variables[[i]] == "ordered_logistic") {
      # construct ordinal variable
      levels <- levels(data[[var]])
      out[[var]] <- ordered(levels[yrep], levels = levels)
    } else if (variables[[i]] == "bernoulli_logit") {
      # construct binary variable
      out[[var]] <- factor(
        ifelse(yrep == 0, "Absent", "Present"), levels = c("Absent", "Present")
      )
    }
  }
  # match missing data pattern in real data
  out <- out[match(data$xd_id, out$xd_id), ]
  if (!identical(out$xd_id, data$xd_id)) {
    stop("Real and synthetic datasets are not aligned on 'xd_id'.")
  }
  for (i in 1:length(variables)) {
    var <- names(variables)[i]
    missing <- is.na(data[[var]])
    out[[var]][missing] <- NA
  }
  # return synthetic dataset with longitude and latitude information
  out |>
    left_join(
      dplyr::select(data, c(xd_id, longitude, latitude)),
      by = "xd_id"
    )
}
