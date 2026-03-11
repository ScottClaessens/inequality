options(tidyverse.quiet = TRUE)
library(crew)
library(targets)
library(tarchetypes)
library(tidyverse)
tar_option_set(
  packages = c("ape", "brms", "cowplot", "ggtree", "phangorn",
               "rnaturalearth", "sf", "tidyverse"),
  controller = crew_controller_local(workers = 2)
)
tar_source()

# pipeline
list(
  # dplace urls
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
  # data files
  tar_target(tree_file, "data/tree/dplace.nxs"),
  # load tree
  tar_target(tree, read.nexus(tree_file)),
  # compute maximum clade credibility tree
  tar_target(mcc_tree, phangorn::mcc(tree)),
  # load dplace data
  tar_target(
    data,
    load_dplace_data(dplace_data_url, dplace_societies_url, mcc_tree)
  ),
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
  # get random tree ids to loop over
  tar_target(tree_ids, sample(1:length(tree), size = 100))
  ## calculate phylogenetic signals for all variables
  #tar_map(
  #  values = tibble(
  #    variable = c(
  #      "class_differentiation", "agriculture", "large_domestic_animals",
  #      "real_property_unigeniture", "movable_property_unigeniture",
  #      "plough_animals", "patrilineality", "monogamy",
  #      "mean_size_local_community", "local_headman", "bridewealth",
  #      "sedentism", "craft_specialisation", "external_warfare_frequency",
  #      "food_storage"
  #    )
  #  ),
  #  tar_target(
  #    signal,
  #    calculate_phylogenetic_signal(data, tree, variable, tree_ids),
  #    pattern = map(tree_ids)
  #  )
  #),
  #tar_target(
  #  phylogenetic_signal,
  #  bind_rows(
  #    signal_class_differentiation, signal_agriculture,
  #    signal_large_domestic_animals, signal_real_property_unigeniture,
  #    signal_movable_property_unigeniture, signal_plough_animals,
  #    signal_patrilineality, signal_monogamy, signal_mean_size_local_community,
  #    signal_local_headman, signal_bridewealth, signal_sedentism,
  #    signal_craft_specialisation, signal_external_warfare_frequency,
  #    signal_food_storage
  #  )
  #),
  ## create table of variables
  #tar_target(
  #  table_variables,
  #  create_table_variables(data, phylogenetic_signal)
  #)
)
