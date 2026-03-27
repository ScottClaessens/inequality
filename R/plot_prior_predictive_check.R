#' Plot prior predictive check
#'
#' Plot predictions for the prior for a particular causal model
#'
#' @param prior_check Fitted model run with prior_only = TRUE
#' @param model String of length one. Causal model. Must be equal to one of the
#'   following: "agriculture", "intergenerational_wealth_transmission",
#'   "family", "population_size", "land_limited", "scalar_stress",
#'   "intergroup_conflict", "bridewealth", "craft_specialisation", or
#'   "food_storage"
#'
#' @returns A patchwork of ggplots
#'
plot_prior_predictive_check <- function(prior_check, model) {
  # get variables list
  variables <- get_variables_list(model)
  # get samples
  prior <- extract_samples(prior_check)
  # loop over variables
  plots <- list()
  for (j in 1:length(variables)) {
    # get variable name and type
    variable_name <- names(variables)[j]
    is_ordinal <- variables[[j]] == "ordered_logistic"
    # get labels for plot
    if (is_ordinal) {
      labels <- levels(prior_check$data[[variable_name]])
    } else {
      labels <- c("Absent", "Present")
    }
    labels <- ifelse(
      labels == "Absence of distinctions",
      "No distinctions",
      labels
    )
    # get prior predictions
    if (is_ordinal) {
      k <- ncol(prior[[paste0("c", j)]]) + 1
    } else {
      k <- 2
    }
    pred <- matrix(nrow = 4000, ncol = k)
    for (i in 1:4000) {
      phi <-
        prior$eta[i, 1, 1, j] +
        prior$dist_v[i, 1, j] +
        prior$tdrift[i, 1, 1, j]
      if (is_ordinal) {
        # for ordinal variable
        pred[i, ] <-
          rethinking::dordlogit(
            x = 1:k,
            phi = phi,
            a = prior[[paste0("c", j)]][i, ]
          )
      } else {
        # for binary variable
        prob <- plogis(phi)
        pred[i, ] <- c(1 - prob, prob)
      }
    }
    # plot prior predictive check
    plots[[j]] <-
      pred |>
      as_tibble(.name_repair = "unique_quiet") |>
      pivot_longer(cols = everything()) |>
      mutate(name = parse_number(str_remove(name, fixed("...")))) |>
      ggplot(
        mapping = aes(
          x = name,
          y = value
        )
      ) +
      ggdist::stat_dist_pointinterval() +
      scale_x_continuous(
        name = NULL,
        breaks = 1:k,
        labels = labels
      ) +
      scale_y_continuous(
        name = "Prior probability",
        limits = c(0, 1)
      ) +
      ggtitle(str_to_sentence(str_replace_all(variable_name, "_", " "))) +
      theme_classic() +
      theme(plot.title = element_text(size = 9))
    if (is_ordinal) {
      plots[[j]] <-
        plots[[j]] +
        theme(
          axis.text.x = element_text(
            size = 8,
            angle = 25,
            hjust = 1,
            vjust = 1
          )
        )
    } else {
      plots[[j]] <-
        plots[[j]] +
        theme(axis.text.x = element_text(size = 8))
    }
    names(plots)[j] <- variable_name
  }
  # layout of plots
  if (length(variables) == 2) {
    design <- "12"
  } else if (length(variables) == 3) {
    design <- "
      12
      3#
    "
  } else if (length(variables) == 4) {
    design <- "
      12
      34
    "
  } else if (length(variables) == 5) {
    design <- "
      111222
      334455
    "
  }
  # put plots together
  out <-
    wrap_plots(plots) +
    plot_layout(
      design = design,
      axis_titles = "collect_y"
    )
  # save and return
  ggsave(
    plot = out,
    filename = paste0("plots/prior/prior_", model, ".pdf"),
    height = ifelse(length(variables) <= 2, 4, 6),
    width = 7
  )
  out
}
