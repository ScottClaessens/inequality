library(targets)
library(tarchetypes)
tar_option_set(packages = c("tidyverse"))
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
  # load dplace data
  tar_target(data, load_dplace_data(dplace_data_url, dplace_societies_url)),
  # plot variable coverage
  tar_target(plot_coverage, plot_variable_coverage(data))
)
