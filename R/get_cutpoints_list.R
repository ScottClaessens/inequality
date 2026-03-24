#' Get list of cutpoints for causal model
#'
#' Get a list of ordinal cutpoints for variables included for a particular
#' causal model
#'
#' @param model String of length one. Causal model. Must be equal to one of the
#'   following: "agriculture", "intergenerational_wealth_transmission",
#'   "family", "population_size", "land_limited", "scalar_stress",
#'   "intergroup_conflict", "bridewealth", "craft_specialisation", or
#'   "food_storage"
#'
#' @returns List
#'
get_cutpoints_list <- function(model) {
  # ordinal cutpoints (null if binary variable)
  cutpoints <- list(
    class_differentiation = c(-0.04, 0.74, 0.90, 2.45),
    agriculture = c(-1.41, -1.18, 0.54, 0.95, 2.12),
    large_domestic_animals = NULL,
    real_property_unigeniture = NULL,
    movable_property_unigeniture = NULL,
    plough_animals = NULL,
    patrilineality = NULL,
    monogamy = NULL,
    mean_size_local_community = c(-1.45, -0.44, 0.24, 0.87, 1.43, 1.61, 2.10),
    local_headman = NULL,
    bridewealth = NULL,
    sedentism = c(-2.61, -1.23, -0.82, -0.76, -0.22, 0.15, 3.60),
    craft_specialisation = NULL,
    external_warfare_frequency = c(-0.33, 1.44),
    food_storage = c(-1.39, 2.30, 2.92, 3.34)
  )
  # return
  vars <- rownames(get_effects_matrix(model))
  cutpoints[vars]
}
