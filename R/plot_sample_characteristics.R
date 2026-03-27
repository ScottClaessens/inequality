#' Plot sample characteristics
#'
#' Plot distributions of variables for the sample of societies from the
#' Ethnographic Atlas
#'
#' @param data Tibble of D-PLACE data
#'
#' @returns A patchwork of ggplots
#'
plot_sample_characteristics <- function(data) {
  # edit class labels for plot
  levels(data$class_differentiation)[1] <- "No distinctions"
  # internal function to plot histogram
  plot_histogram <- function(variable) {
    # get plot title
    title <- str_to_sentence(str_replace_all(variable, "_", " "))
    # plot
    out <-
      data |>
      filter(!is.na(!!sym(variable))) |>
      ggplot(mapping = aes(x = !!sym(variable))) +
      geom_col(
        fill = "skyblue4",
        stat = "count"
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(
        title = title,
        x = NULL,
        y = "Count"
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 7),
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 6)
      )
    if ("ordered" %in% class(data[[variable]])) {
      out +
        theme(
          axis.text.x = element_text(
            angle = 25,
            hjust = 1,
            vjust = 1
          )
        )
    } else {
      out
    }
  }
  # variables on each row
  rows <-
    list(
      c("class_differentiation", "agriculture", "food_storage"),
      c("mean_size_local_community", "sedentism", "external_warfare_frequency"),
      c("large_domestic_animals", "real_property_unigeniture",
        "movable_property_unigeniture", "plough_animals"),
      c("patrilineality", "monogamy", "local_headman", "bridewealth", "craft_specialisation")
    )
  # combine plots
  out <-
    wrap_plots(
      lapply(
        rows,
        function(x) wrap_plots(
          lapply(x, plot_histogram),
          nrow = 1,
          axis_titles = "collect_y"
        )
      ),
      nrow = length(rows)
    )
  # save
  ggsave(
    filename = "plots/sample_characteristics.pdf",
    plot = out,
    height = 5,
    width = 6
  )
  out
}
