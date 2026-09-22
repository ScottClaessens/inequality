plot_standardised_effects <- function(
    A_std_agriculture,
    A_std_intergenerational_wealth_transmission,
    A_std_family,
    A_std_population_size,
    A_std_plough_animals,
    A_std_scalar_stress,
    A_std_intergroup_conflict,
    A_std_bridewealth,
    A_std_craft_specialisation,
    A_std_food_storage
  ) {

  # internal function to plot standardised effects
  plot_effect <- function(names, values_list, label) {
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
      unnest(value) |>
      ggplot(aes(y = fct_rev(name))) +
      tidybayes::stat_slab(
        aes(x = value),
        normalize = "none"
      ) +
      geom_pointrange(
        aes(
          x = median,
          xmin = lower,
          xmax = upper
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

  # agriculture model
  pA <- plot_effect(
    names = "Agriculture -> Inequality",
    values_list = list(A_std_agriculture[, 1, 2]),
    label = "a"
  )

  # intergenerational wealth transmission model
  pB <- plot_effect(
    names = c(
      "Agriculture -> Real unigeniture",
      "Agriculture -> Movable unigeniture",
      "Large animals -> Real unigeniture",
      "Large animals -> Movable unigeniture",
      "Real unigeniture -> Inequality",
      "Movable unigeniture -> Inequality"
    ),
    values_list = list(
      A_std_intergenerational_wealth_transmission[, 4, 2],
      A_std_intergenerational_wealth_transmission[, 5, 2],
      A_std_intergenerational_wealth_transmission[, 4, 3],
      A_std_intergenerational_wealth_transmission[, 5, 3],
      A_std_intergenerational_wealth_transmission[, 1, 4],
      A_std_intergenerational_wealth_transmission[, 1, 5]
    ),
    label = "b"
  )

  # family model
  pC <- plot_effect(
    names = c(
      "Agriculture -> Patrilineal descent",
      "Patrilineal descent -> Monogamy",
      "Monogamy -> Inequality"
    ),
    values_list = list(
      A_std_family[, 3, 2],
      A_std_family[, 4, 3],
      A_std_family[, 1, 4]
    ),
    label = "c"
  )

  # population size model
  pD <- plot_effect(
    names = c(
      "Agriculture -> Population size",
      "Population size -> Inequality"
    ),
    values_list = list(
      A_std_population_size[, 3, 2],
      A_std_population_size[, 1, 3]
    ),
    label = "d"
  )

  # plough animals model
  pE <- plot_effect(
    names = c(
      "Agriculture -> Plough animals",
      "Plough animals -> Real unigeniture",
      "Real unigeniture -> Inequality"
    ),
    values_list = list(
      A_std_plough_animals[, 3, 2],
      A_std_plough_animals[, 4, 3],
      A_std_plough_animals[, 1, 4]
    ),
    label = "e"
  )

  # scalar stress model
  pF <- plot_effect(
    names = c(
      "Population size -> Leadership",
      "Leadership -> Population size",
      "Leadership -> Inequality"
    ),
    values_list = list(
      A_std_scalar_stress[, 3, 2],
      A_std_scalar_stress[, 2, 3],
      A_std_scalar_stress[, 1, 3]
    ),
    label = "f"
  )

  # intergroup conflict model
  pG <- plot_effect(
    names = c(
      "Intergroup conflict -> Leadership",
      "Leadership -> Inequality"
    ),
    values_list = list(
      A_std_intergroup_conflict[, 3, 2],
      A_std_intergroup_conflict[, 1, 3]
    ),
    label = "g"
  )

  # bridewealth model
  pH <- plot_effect(
    names = "Bridewealth -> Inequality",
    values_list = list(A_std_bridewealth[, 1, 2]),
    label = "h"
  )

  # craft specialisation model
  pI <- plot_effect(
    names = "Craft specialisation -> Inequality",
    values_list = list(A_std_craft_specialisation[, 1, 2]),
    label = "i"
  )

  # food storage model
  pJ <- plot_effect(
    names = c(
      "Sedentism -> Food storage",
      "Food storage -> Sedentism",
      "Sedentism -> Inequality",
      "Food storage -> Inequality"
    ),
    values_list = list(
      A_std_food_storage[, 3, 2],
      A_std_food_storage[, 2, 3],
      A_std_food_storage[, 1, 2],
      A_std_food_storage[, 1, 3]
    ),
    label = "j"
  )

  # get left plot
  left <-
    pA / pB / pC / pD / pE +
    plot_layout(
      heights = c(1, 6, 3, 2, 3),
      axes = "collect_x",
      axis_titles = "collect_x"
    )

  # get right plot
  right <-
    pF / pG / pH / pI / pJ +
    plot_layout(
      heights = c(3, 2, 1, 1, 4),
      axes = "collect_x",
      axis_titles = "collect_x"
    )

  # put together
  out <- left | right

  # save
  ggsave(
    file = "plots/standardised_effects.pdf",
    plot = out,
    height = 4,
    width = 7
  )

  # cleanup
  rm(plot_effect, pA, pB, pC, pD, pE)

  # return
  out

}
