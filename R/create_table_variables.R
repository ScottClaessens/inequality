#' Create table of Ethnographic Atlas variables
#'
#' Create table of Ethnographic Atlas variables used in the study, including
#' information on the original EA code, the type of variable (binary or
#' ordinal), any recoding decisions, the proportion of observed data, and
#' estimates of phylogenetic signal.
#'
#' @param data Tibble of D-PLACE data
#'
#' @returns A tibble
#'
create_table_variables <- function(data, phylogenetic_signal) {
  # get counts and proportions of observed data
  prop_observed <-
    data |>
    summarise(
      across(
        class_differentiation:food_storage,
        function(x) {
          paste0(sum(!is.na(x)), " (", floor(mean(!is.na(x)) * 100), "%)")
        }
      )
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Name",
      values_to = "N societies with observed data (%)"
    )
  # function for printing phylogenetic signal with percentile interval
  print_lambda <- function(x, prob = 0.95) {
    paste0(
      format(round(mean(x), digits = 2), nsmall = 2),
      " [",
      format(round(quantile(x, (1 - prob) / 2), digits = 2), nsmall = 2),
      ", ",
      format(round(quantile(x, 1 - (1 - prob) / 2), digits = 2), nsmall = 2),
      "]"
    )
  }
  # wrangle phylogenetic signals
  phylogenetic_signals <-
    phylogenetic_signal |>
    unnest(phylogenetic_signal) |>
    group_by(variable) |>
    summarise(`Phylogenetic signal` = print_lambda(phylogenetic_signal))
  # create table
  tibble(
    Name = colnames(data)[7:21],
    Code = c(
      "EA066", "EA028", "EA040", "EA075", "EA077", "EA039", "EA043", "EA009",
      "EA031", "EA072", "EA006", "EA030", "EA055-EA060", "SCCS892", "SCCS20"
    ),
    Type = ifelse(
      sapply(data, function(x) class(x)[[1]])[7:21] == "factor",
      "Binary", "Ordinal"
    ),
    Description = c(
      "Extent of class differentiation",
      "Intensity of agricultural cultivation",
      paste0(
        "Presence/absence of large domestic animals including ",
        "equine, deer, camelids, and bovine"
      ),
      "Presence/absence of inheritance of real property by a single heir",
      "Presence/absence of inheritance of movable property by a single heir",
      paste0(
        "Presence/absence of animals employed in plow cultivation ",
        "prior to the contact period"
      ),
      "Presence/absence of patrilineality as the major mode of descent",
      "Presence/absence of monogamy as the marital composition of family units",
      "Average population size of local communities",
      "Presence/absence of office of local headman",
      paste0(
        "Presence/absence of bride-wealth, bride-price, bride-service, ",
        "or token bride-price as the prevailing type of transfer ",
        "or exchange at marriage"
      ),
      "Extent of sedentism in the prevailing type of settlement pattern",
      paste0(
        "Presence/absence of craft specialisation in any of the following: ",
        "metal working, weaving, leather working, pottery making, ",
        "boat building, or house construction"
      ),
      "Frequency of external warfare",
      "Extent of food storage"
    )
  ) |>
    left_join(prop_observed) |>
    left_join(phylogenetic_signals, by = c("Name" = "variable")) |>
    mutate(
      Name = str_to_sentence(str_replace_all(Name, "_", " ")),
      Name = ifelse(
        Name == "Mean size local community", "Population size", Name
      ),
      Name = ifelse(Name == "Local headman", "Leadership", Name)
    )
}
