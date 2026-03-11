#' Load and wrangle data from D-PLACE
#'
#' Load and wrangle data on inequality and other variables from the Ethnographic
#' Atlas and Standard Cross-Cultural Sample via the online database D-PLACE
#' (release v3.3.0). The dataset is filtered to 1127 societies that can be
#' linked to the phylogenetic tree.
#'
#' @details The dataset produced by this function is a tibble with 1127
#'   observations and 21 variables:
#' \describe{
#'  \item{soc_id}{Character, society ID}
#'  \item{xd_id}{Character, cross-dataset ID (see
#'    https://d-place.org/glossary#q9)}
#'  \item{society}{Name of the society}
#'  \item{region}{Region of the society}
#'  \item{latitude}{Latitude of the society}
#'  \item{longitude}{Longitude of the society}
#'  \item{class_differentiation}{Ordered factor (five levels), extent of class
#'    differentiation; coded from EA066}
#'  \item{agriculture}{Ordered factor (six levels), intensity of agricultural
#'    cultivation; coded from EA028}
#'  \item{large_domestic_animals}{Factor, presence/absence of large domestic
#'    animals including equine, deer, camelids, and bovine; coded from EA040}
#'  \item{real_property_unigeniture}{Factor, presence/absence of real property
#'    unigeniture (i.e., inheritance of real property by a single heir); coded
#'    from EA075}
#'  \item{movable_property_unigeniture}{Factor, presence/absence of movable
#'    property unigeniture (i.e., inheritance of movable property by a single
#'    heir); coded from EA077}
#'  \item{plough_animals}{Factor, presence/absence of animals employed in plow
#'    cultivation prior to the contact period; coded from EA039}
#'  \item{patrilineality}{Factor, presence/absence of patrilineality as the
#'    major mode of descent; coded from EA043}
#'  \item{monogamy}{Factor, presence/absence of monogamy as the marital
#'    composition of family units; coded from EA009}
#'  \item{mean_size_local_community}{Ordered factor (eight levels), average
#'    population of local communities; coded from EA031}
#'  \item{local_headman}{Factor, presence/absence of office of local headman;
#'    coded from EA072}
#'  \item{bridewealth}{Factor, presence/absence of bride-wealth, bride-price,
#'    bride-service, or token bride-price as the prevailing type of transfer
#'    or exchange at marriage; coded from EA006}
#'  \item{sedentism}{Ordered factor (eight levels), extent of sedentism in the
#'    prevailing type of settlement pattern; coded from EA030}
#'  \item{craft_specialisation}{Factor, presence/absence of craft specialisation
#     in any of the following: metal working, weaving, leather working, pottery
#'    making, boat building, or house construction; coded from EA055-EA060}
#'  \item{external_warfare_frequency}{Ordered factor (three levels), frequency
#'    of external warfare (attacking); recoded from SCCS892}
#'  \item{food_storage}{Ordered factor (five levels), extent of food storage;
#'    coded from SCCS20}
#' }
#'
#' @param dplace_data_url URL to access cldf/data.csv from D-PLACE v3.3.0
#' @param dplace_societies_url URL to access cldf/societies.csv from D-PLACE
#'   v3.3.0
#' @param mcc_tree Maximum clade credibility tree of D-PLACE societies used to
#'   filter the dataset
#'
#' @returns A tibble
#'
load_dplace_data <- function(dplace_data_url, dplace_societies_url, mcc_tree) {
  # load csv files
  data <- read.csv(file = dplace_data_url)
  societies <- read.csv(file = dplace_societies_url)
  # wrangle ethnographic atlas data (n = 1290)
  ea <- wrangle_ea(data, societies)
  # wrangle sccs data (n = 186)
  sccs <- wrangle_sccs(data, societies)
  # join datasets
  joined <- left_join(ea, sccs, by = "xd_id")
  # filter to societies in phylogenetic tree (n = 1125)
  filter(joined, xd_id %in% mcc_tree$tip.label)
}

#' Wrangle Ethnographic Atlas data from D-PLACE
#'
#' This function wrangles the data from the Ethnographic Atlas by pivoting the
#' dataset wider, converting variables to ordinal/binary, and removing a
#' duplicate case (Nd55).
#'
#' @param data Data frame of cldf/data.csv file from D-PLACE
#' @param societies Data frame of cldf/societies.csv file from D-PLACE
#'
#' @returns A tibble
#'
wrangle_ea <- function(data, societies) {
  # ordered levels
  levels_EA028 <- c("No agriculture", "Casual", "Extensive/shifting",
                    "Horticulture", "Intensive", "Intensive irrigated")
  levels_EA030 <- c("Nomadic", "Seminomadic", "Semisedentary", "Impermanent",
                    "Dispersed homesteads", "Hamlets", "Villages/towns",
                    "Complex permanent")
  levels_EA031 <- c("<50", "50-99", "100-199", "200-399", "400-1000",
                    "1000-5000", "5000-50000", "50000+")
  levels_EA066 <- c("Absence of distinctions", "Wealth distinctions",
                    "Elite stratification", "Dual stratification",
                    "Complex stratification")
  # absence codes
  absent_EA006 <- c("Gift exchange", "Woman exchange", "Insignificant", "Dowry")
  absent_EA009 <- c("Limited polygyny", "Polygyny, sororal cohabit",
                    "Polygyny, sororal separate quarters",
                    "Polygyny, non-sororal separate quarters",
                    "Polygyny, non-sororal cohabit", "Polyandrous")
  absent_EA039 <- c("Absent", "Not aboriginal but present")
  absent_EA040 <- c("Absence or near absence", "Pigs", "Sheep/goats")
  absent_EA043 <- c("Duolateral", "Matrilineal", "Quasi-lineages", "Ambilineal",
                    "Bilateral", "Mixed")
  absent_EA072 <- c("Absence of office")
  absent_EA075 <- c("No inher.of real property", "Equally distributed")
  absent_EA077 <- c("No inher.of real property", "Equally distributed")
  absent_craft <- c("Junior age", "Senior age", "Industrial", "Most adults",
                    "Activity is absent")
  # function to code absent/present values
  code_absence_presence <- function(variable, absent_values) {
    ifelse(
      variable == "", NA, ifelse(
        variable %in% absent_values, "Absent", "Present"
      )
    )
  }
  # wrangle ethnographic atlas data
  data |>
    # filter to ethnographic atlas data only
    left_join(societies, by = c("Soc_ID" = "ID")) |>
    filter(Contribution_ID == "dplace-dataset-ea") |>
    # pivot wider
    pivot_wider(
      id_cols = c(Soc_ID, xd_id, Name, Latitude, Longitude, region),
      names_from = Var_ID,
      values_from = Value
    ) |>
    # retain variables
    transmute(
      soc_id                       = Soc_ID,
      xd_id                        = xd_id,
      society                      = Name,
      region                       = region,
      latitude                     = Latitude,
      longitude                    = Longitude,
      class_differentiation        = ordered(EA066, levels = levels_EA066),
      agriculture                  = ordered(EA028, levels = levels_EA028),
      large_domestic_animals       = code_absence_presence(EA040, absent_EA040),
      real_property_unigeniture    = code_absence_presence(EA075, absent_EA075),
      movable_property_unigeniture = code_absence_presence(EA077, absent_EA077),
      plough_animals               = code_absence_presence(EA039, absent_EA039),
      patrilineality               = code_absence_presence(EA043, absent_EA043),
      monogamy                     = code_absence_presence(EA009, absent_EA009),
      mean_size_local_community    = ordered(EA031, levels = levels_EA031),
      local_headman                = code_absence_presence(EA072, absent_EA072),
      bridewealth                  = code_absence_presence(EA006, absent_EA006),
      sedentism                    = ordered(EA030, levels = levels_EA030),
      craft_metal_working          = code_absence_presence(EA055, absent_craft),
      craft_weaving                = code_absence_presence(EA056, absent_craft),
      craft_leather_working        = code_absence_presence(EA057, absent_craft),
      craft_pottery_making         = code_absence_presence(EA058, absent_craft),
      craft_boat_building          = code_absence_presence(EA059, absent_craft),
      craft_house_construction     = code_absence_presence(EA060, absent_craft)
    ) |>
    # calculate craft specialisation variable
    rowwise() |>
    mutate(
      craft_specialisation = ifelse(
        # if all craft_ columns are missing, set to NA
        all(is.na(c_across(starts_with("craft_")))), NA,
        # otherwise, set as "Present" if at least one craft column is present
        ifelse(
          any(c_across(starts_with("craft_")) == "Present", na.rm = TRUE),
          "Present", "Absent"
        )
      )
    ) |>
    ungroup() |>
    dplyr::select(
      !c(craft_metal_working, craft_weaving, craft_leather_working,
         craft_pottery_making, craft_boat_building, craft_house_construction)
    ) |>
    # remove one xd_id duplicate
    filter(soc_id != "Nd55") |>
    # absent/present as factor
    mutate(
      across(
        where(is.character) & !c(soc_id, xd_id, society, region),
        function(x) factor(x, levels = c("Absent", "Present"))
      )
    )
}

#' Wrangle Standard Cross-Cultural Sample data from D-PLACE
#'
#' This function wrangles the data from the Standard Cross-Cultural Sample by
#' pivoting the dataset wider and recoding two ordinal variables.
#'
#' @param data Data frame of cldf/data.csv file from D-PLACE
#' @param societies Data frame of cldf/societies.csv file from D-PLACE
#'
#' @returns A tibble
#'
wrangle_sccs <- function(data, societies) {
  # ordered levels
  levels_SCCS892 <- c("Infrequent", "Frequent", "Continual")
  levels_SCCS20 <- c("None", "Individual households", "Communal facilities",
                     "Political agent controlled", "Economic agent controlled")
  # wrangle standard cross-cultural sample data
  data |>
    # filter to sccs data only
    left_join(societies, by = c("Soc_ID" = "ID")) |>
    filter(Contribution_ID == "dplace-dataset-sccs") |>
    # pivot wider
    pivot_wider(
      id_cols = c(Soc_ID, xd_id),
      names_from = Var_ID,
      values_from = Value
    ) |>
    # retain variables
    transmute(
      xd_id = xd_id,
      external_warfare_frequency = ordered(SCCS892, levels = levels_SCCS892),
      food_storage = ordered(SCCS20, levels = levels_SCCS20)
    )
}
