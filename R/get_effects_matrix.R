#' Get effects matrix for causal model
#'
#' Get a matrix of autoregressive and cross-lagged effects for a particular
#' causal model
#'
#' @param model String of length one. Causal model. Must be equal to one of the
#'   following: "agriculture", "intergenerational_wealth_transmission",
#'   "family", "population_size", "land_limited", "scalar_stress",
#'   "intergroup_conflict", "bridewealth", "craft_specialisation", or
#'   "food_storage"
#'
#' @returns Matrix of logical values
#'
get_effects_matrix <- function(model) {

  if (model == "agriculture") {

    variables <- c("class_differentiation", "agriculture")
    effects_matrix <- c(TRUE, TRUE,
                        FALSE, TRUE)

  } else if (model == "intergenerational_wealth_transmission") {

    variables <- c("class_differentiation", "agriculture",
                   "large_domestic_animals", "real_property_unigeniture",
                   "movable_property_unigeniture")
    effects_matrix <- c(TRUE, FALSE, FALSE, TRUE, TRUE,
                        FALSE, TRUE, FALSE, FALSE, FALSE,
                        FALSE, FALSE, TRUE, FALSE, FALSE,
                        FALSE, TRUE, TRUE, TRUE, FALSE,
                        FALSE, TRUE, TRUE, FALSE, TRUE)

  } else if (model == "family") {

    variables <- c("class_differentiation", "agriculture",
                   "patrilineality", "monogamy")
    effects_matrix <- c(TRUE, TRUE, FALSE, TRUE,
                        FALSE, TRUE, FALSE, FALSE,
                        FALSE, TRUE, TRUE, FALSE,
                        FALSE, FALSE, TRUE, TRUE)

  } else if (model == "population_size") {

    variables <- c("class_differentiation", "agriculture",
                   "mean_size_local_community")
    effects_matrix <- c(TRUE, FALSE, TRUE,
                        FALSE, TRUE, FALSE,
                        FALSE, TRUE, TRUE)

  } else if (model == "land_limited") {

    variables <- c("class_differentiation", "agriculture",
                   "plough_animals", "real_property_unigeniture")
    effects_matrix <- c(TRUE, FALSE, FALSE, TRUE,
                        FALSE, TRUE, FALSE, FALSE,
                        FALSE, TRUE, TRUE, FALSE,
                        FALSE, FALSE, TRUE, TRUE)

  } else if (model == "scalar_stress") {

    variables <- c("class_differentiation", "mean_size_local_community",
                   "local_headman")
    effects_matrix <- c(TRUE, FALSE, TRUE,
                        TRUE, TRUE, FALSE,
                        FALSE, TRUE, TRUE)

  } else if (model == "intergroup_conflict") {

    variables <- c("class_differentiation", "external_warfare_frequency",
                   "local_headman")
    effects_matrix <- c(TRUE, FALSE, TRUE,
                        FALSE, TRUE, FALSE,
                        FALSE, TRUE, TRUE)

  } else if (model == "bridewealth") {

    variables <- c("class_differentiation", "bridewealth")
    effects_matrix <- c(TRUE, TRUE,
                        FALSE, TRUE)

  } else if (model == "craft_specialisation") {

    variables <- c("class_differentiation", "craft_specialisation")
    effects_matrix <- c(TRUE, TRUE,
                        FALSE, TRUE)

  } else if (model == "food_storage") {

    variables <- c("class_differentiation", "sedentism", "food_storage")
    effects_matrix <- c(TRUE, TRUE, TRUE,
                        FALSE, TRUE, TRUE,
                        FALSE, TRUE, TRUE)

  } else {

    stop("Argument 'model' not recognised.")

  }

  # return matrix
  matrix(
    effects_matrix,
    byrow = TRUE,
    nrow = length(variables),
    ncol = length(variables),
    dimnames = list(variables, variables)
  )
}

