#' Plot results of fitting the model to synthetic data
#'
#' Plot the posterior estimates from the model fitted to synthetic data
#' generated under a particular causal model
#'
#' @param synthetic_fit A coevfit object. Model fitted to synthetic data.
#' @param model String of length one. Causal model. Must be equal to one of the
#'   following: "agriculture", "intergenerational_wealth_transmission",
#'   "family", "population_size", "land_limited", "scalar_stress",
#'   "intergroup_conflict", "bridewealth", "craft_specialisation", or
#'   "food_storage"
#'
#' @returns A ggplot object
#'
plot_synthetic_fit <- function(synthetic_fit, model) {
  # get posterior draws
  post <-
    as_draws_df(synthetic_fit$fit) |>
    dplyr::select(
      starts_with("A[") |
        starts_with("Q[") |
        starts_with("b[") |
        starts_with(".")
    ) |>
    # remove columns with fixed parameters
    dplyr::select(where(function(x) var(x) != 0)) |>
    # convert to long format
    pivot_longer(
      cols = !c(.chain, .iteration, .draw),
      names_to = "parameter"
    )
  # get true parameter values
  true_values <-
    tibble(par = unique(post$parameter)) |>
    rowwise() |>
    mutate(
      unique_nums = sum(str_count(par, as.character(1:5)) != 0),
      true = ifelse(str_starts(par, fixed("b[")), 0, NA),
      true = ifelse(str_starts(par, fixed("Q[")), 2, true),
      true = ifelse(str_starts(par, fixed("A[")) & unique_nums == 1, -1, true),
      true = ifelse(str_starts(par, fixed("A[")) & unique_nums == 2, 4, true)
    ) |>
    transmute(
      parameter = par,
      true_value = true
    )
  # plot
  out <-
    ggplot() +
    geom_vline(
      xintercept = 0,
      linetype = "dashed"
    ) +
    ggdist::stat_pointinterval(
      data = post,
      mapping = aes(
        x = value,
        y = fct_rev(parameter)
      )
    ) +
    geom_point(
      data = true_values,
      mapping = aes(
        x = true_value,
        y = fct_rev(parameter)
      ),
      colour = "red",
      shape = 17
    ) +
    labs(
      title = str_to_sentence(str_replace_all(model, "_", " ")),
      x = "Posterior estimate",
      y = "Parameter"
    ) +
    theme_classic()
  # save and return
  ggsave(
    plot = out,
    filename = paste0("plots/simulation/simulation_", model, ".pdf"),
    width = 4,
    height = (3 + length(synthetic_fit$variables)) / 2 # scale height
  )
  rm(synthetic_fit)
  out
}
