#' Get list of variables for causal model
#'
#' Get a list of variables and associated response distributions for a
#' particular causal model
#'
#' @param model String of length one. Causal model. Must be equal to one of the
#'   following: "agriculture", "intergenerational_wealth_transmission",
#'   "family", "population_size", "land_limited", "scalar_stress",
#'   "intergroup_conflict", "bridewealth", "craft_specialisation", or
#'   "food_storage"
#'
#' @returns List
#'
get_variables_list <- function(model) {
  # get cutpoints list
  cutpoints <- get_cutpoints_list(model)
  # construct variables list
  variables <- list()
  for (i in 1:length(cutpoints)) {
    var <- names(cutpoints)[i]
    if (!is.null(cutpoints[[i]])) {
      variables[[var]] <- "ordered_logistic"
    } else {
      variables[[var]] <- "bernoulli_logit"
    }
  }
  # return
  variables
}
