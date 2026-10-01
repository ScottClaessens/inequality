#' Plot standardised effects from exploratory models
#'
#' @param A_std_intergenerational_wealth_transmission2 Standardised effects from
#'   the intergenerational wealth transmission model
#' @param A_std_plough_animals2 Standardised effects from the plough animals
#'   model
#'
#' @returns A patchwork of ggplots
#'
plot_standardised_effects_exploratory <- function(
    A_std_intergenerational_wealth_transmission2,
    A_std_plough_animals2
  ) {

  # internal function to plot standardised effects
  plot_effect <- function(names, values_list, label) {

    # get data for plot
    data <-
      tibble(
        name = names,
        value = values_list
      ) |>
      rowwise() |>
      mutate(
        name = factor(name, levels = names),
        median = median(value),
        lower = coda::HPDinterval(coda::mcmc(value), prob = 0.9)[1],
        upper = coda::HPDinterval(coda::mcmc(value), prob = 0.9)[2]
      ) |>
      unnest(value)

    # plot
    ggplot() +
      tidybayes::stat_slab(
        data = data,
        aes(
          x = value,
          y = fct_rev(name)
        ),
        normalize = "none"
      ) +
      geom_pointrange(
        data = data |>
          group_by(name) |>
          summarise(
            median = unique(median),
            lower = unique(lower),
            upper = unique(upper)
          ),
        aes(
          x = median,
          xmin = lower,
          xmax = upper,
          y = fct_rev(name)
        ),
        size = 0.2
      ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        size = 0.2
      ) +
      xlim(c(-4, 8)) +
      labs(
        x = "Cross-selection effect (std.)",
        y = NULL,
        tag = label
      ) +
      theme_classic() +
      theme(
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        plot.tag = element_text(size = 6, face = "bold")
      )

  }

  # intergenerational wealth transmission model
  pB <- plot_effect(
    names = c(
      "Agriculture -> Real inheritance",
      "Agriculture -> Movable inheritance",
      "Large animals -> Real inheritance",
      "Large animals -> Movable inheritance",
      "Real inheritance -> Inequality",
      "Movable inheritance -> Inequality"
    ),
    values_list = list(
      A_std_intergenerational_wealth_transmission2[, 4, 2],
      A_std_intergenerational_wealth_transmission2[, 5, 2],
      A_std_intergenerational_wealth_transmission2[, 4, 3],
      A_std_intergenerational_wealth_transmission2[, 5, 3],
      A_std_intergenerational_wealth_transmission2[, 1, 4],
      A_std_intergenerational_wealth_transmission2[, 1, 5]
    ),
    label = "b"
  )

  # plough animals model
  pE <- plot_effect(
    names = c(
      "Agriculture -> Plough animals",
      "Plough animals -> Real inheritance",
      "Real inheritance -> Inequality"
    ),
    values_list = list(
      A_std_plough_animals2[, 3, 2],
      A_std_plough_animals2[, 4, 3],
      A_std_plough_animals2[, 1, 4]
    ),
    label = "e"
  )

  # get plot
  out <-
    pB / pE +
    plot_layout(
      heights = c(6, 3),
      axes = "collect_x",
      axis_titles = "collect_x"
    )

  # save
  ggsave(
    file = "plots/standardised_effects_exploratory.pdf",
    plot = out,
    height = 3,
    width = 4
  )

  # cleanup
  rm(
    A_std_intergenerational_wealth_transmission2,
    A_std_plough_animals2,
    plot_effect,
    pB, pE
  )

  # return
  out

}
