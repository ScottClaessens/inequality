options(tidyverse.quiet = TRUE)
library(crew)
library(targets)
library(tarchetypes)
library(tidyverse)
tar_option_set(
  packages = c("ape", "brms", "cmdstanr", "coevolve", "cowplot", "ggdist",
               "ggtree", "gt", "patchwork", "phangorn", "posterior",
               "rnaturalearth", "sf", "tidyverse", "withr"),
  controller = crew_controller_local(workers = 2)
)
tar_source()

# pipeline
list(
  # get data urls
  tar_target(
    dplace_data_url,
    paste0(
      "https://raw.githubusercontent.com/D-PLACE/dplace-cldf/",
      "6c2008c187a297d1955b41d8ae80d8e31d404f6c/cldf/data.csv"
    ),
    format = "url"
  ),
  tar_target(
    dplace_societies_url,
    paste0(
      "https://raw.githubusercontent.com/D-PLACE/dplace-cldf/",
      "6c2008c187a297d1955b41d8ae80d8e31d404f6c/cldf/societies.csv"
    ),
    format = "url"
  ),
  tar_target(
    glottolog_languages_url,
    paste0(
      "https://raw.githubusercontent.com/glottolog/glottolog-cldf/",
      "072ca0d0410039fb8b779be8fc165bac575d2cda/cldf/languages.csv"
    ),
    format = "url"
  ),
  # get data file paths
  tar_target(tree_file, "data/tree/dplace.nxs", format = "file"),
  # load tree
  tar_target(tree, load_tree(tree_file)),
  # compute maximum clade credibility tree
  tar_target(mcc_tree, phangorn::mcc(tree)),
  # load dplace data
  tar_target(
    data,
    load_dplace_data(
      dplace_data_url, dplace_societies_url,
      glottolog_languages_url, mcc_tree
    )
  ),
  # plot sample characteristics
  tar_target(plot_sample, plot_sample_characteristics(data)),
  # plot variable coverage
  tar_target(plot_coverage, plot_variable_coverage(data)),
  # plot world map
  tar_target(plot_world, plot_world_map(data)),
  # plot maximum clade credibility tree
  tar_target(plot_tree, ggtree(mcc_tree, layout = "circular")),
  # plot gdpm schematic
  tar_target(plot_gdpm, plot_gdpm_algorithm()),
  # plot all methods together
  tar_target(plot_methods, plot_all_methods(plot_world, plot_tree, plot_gdpm)),
  # plot correlation between linguistic and geographic distances
  tar_target(plot_cor, plot_linguistic_spatial_distance(data, mcc_tree)),
  # get random tree ids
  tar_target(tree_ids, sample(1:length(tree), size = 100)),
  # loop over all variables
  tar_map(
    values = tibble(
      variable = c(
        "class_differentiation", "agriculture", "large_domestic_animals",
        "real_property_unigeniture", "movable_property_unigeniture",
        "plough_animals", "patrilineality", "monogamy",
        "mean_size_local_community", "local_headman", "bridewealth",
        "sedentism", "craft_specialisation", "external_warfare_frequency",
        "food_storage"
      )
    ),
    # plot variable on tree
    tar_target(plot_trait, plot_trait_on_tree(data, mcc_tree, variable)),
    # calculate phylogenetic signal for variable
    tar_target(
      signal,
      calculate_phylogenetic_signal(data, tree, variable, tree_ids),
      pattern = map(tree_ids)
    )
  ),
  # combine phylogenetic signal estimates
  tar_target(
    phylogenetic_signal,
    bind_rows(
      signal_class_differentiation, signal_agriculture,
      signal_large_domestic_animals, signal_real_property_unigeniture,
      signal_movable_property_unigeniture, signal_plough_animals,
      signal_patrilineality, signal_monogamy, signal_mean_size_local_community,
      signal_local_headman, signal_bridewealth, signal_sedentism,
      signal_craft_specialisation, signal_external_warfare_frequency,
      signal_food_storage
    )
  ),
  # create table of variables
  tar_target(
    table_variables,
    create_table_variables(data, phylogenetic_signal)
  ),
  # plot prior predictive check for spatial gaussian processes
  tar_target(plot_spatial_prior, plot_spatial_gp_prior()),
  # create subsample of dummy data (n = 20) for prior predictive check
  tar_target(
    data_subsample,
    data[sample(which(!is.na(data$food_storage)), size = 20), ]
  ),
  # loop over causal models
  tar_map(
    values = tibble(
      model = c(
        "agriculture", "intergenerational_wealth_transmission", "family",
        "population_size", "land_limited", "scalar_stress",
        "intergroup_conflict", "bridewealth", "craft_specialisation",
        "food_storage"
      )
    ),
    # run prior predictive check
    tar_target(prior_check, fit_model(data_subsample, mcc_tree, model,
                                      prior_only = TRUE)),
    # plot prior predictive check
    tar_target(plot_prior, plot_prior_predictive_check(prior_check, model)),
    # generate synthetic data
    tar_target(synthetic_data, generate_synthetic_data(data, mcc_tree, model)),
    # fit model to synthetic data
    tar_target(synthetic_fit, fit_model(synthetic_data, mcc_tree, model,
                                        spatial_control = FALSE)),
    # plot synthetic results
    tar_target(plot_synthetic, plot_synthetic_fit(synthetic_fit, model))
  ),
  # generate manuscript
  tar_quarto(manuscript, "quarto/manuscript.qmd", quiet = FALSE),
  # print session info
  tar_target(
    sessionInfo,
    writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
  )
)
