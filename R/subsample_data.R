#' Sub-sample D-PLACE data
#'
#' Sub-sample data from D-PLACE while ensuring that every factor level of every
#' categorical variable appears at least once in the sample
#'
#' @param data Tibble of D-PLACE data
#'
#' @returns A tibble of sub-sampled data
#'
subsample_data <- function(data) {

  # get one row for every factor level of every factor variable
  required_rows <-
    data |>
    mutate(row_id = row_number()) |>
    mutate(across(where(is.ordered), function(x) factor(x, ordered = FALSE))) |>
    pivot_longer(
      cols = class_differentiation:food_storage,
      names_to = "variable",
      values_to = "level"
    ) |>
    group_by(variable, level) |>
    slice_sample(n = 1) |>
    ungroup() |>
    distinct(row_id) |>
    pull(row_id)

  # return sub-sampled data (n = 67)
  data[required_rows, ]

}
