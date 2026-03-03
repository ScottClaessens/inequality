library(targets)
library(tarchetypes)
tar_option_set(
  packages = c("ape", "cowplot", "ggtree", "phangorn", "rnaturalearth",
               "sf", "tidyverse")
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
  tar_target(plot_methods, plot_all_methods(plot_world, plot_tree, plot_gdpm))
)
