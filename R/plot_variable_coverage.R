#' Plot coverage for D-PLACE variables
#'
#' Plot proportions of observed data for variables from the Ethnographic Atlas
#' and Standard Cross-Cultural Sample
#'
#' @param data Tibble of D-PLACE data
#'
#' @returns A ggplot object
#'
plot_variable_coverage <- function(data) {
  # counts of observed data for D-PLACE variables
  counts <- apply(data, 2, function(x) sum(!is.na(x)))[-c(1:6)]
  names(counts) <- str_to_sentence(str_replace_all(names(counts), "_", " "))
  # plot
  p <-
    tibble(
      variable = factor(names(counts), levels = names(counts)),
      count = as.vector(counts)
    ) |>
    ggplot(
      mapping = aes(
        x = variable,
        y = count
      )
    ) +
    xlab(NULL) +
    geom_col(fill = "lightblue4") +
    geom_hline(
      yintercept = 1290,
      linetype = "dashed"
    ) +
    scale_y_continuous(
      name = "Number of societies\nwith observed data",
      limits = c(0, 1350),
      breaks = seq(0, 1250, by = 250),
      expand = c(0, 0)
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  # save
  ggsave(
    filename = "plots/coverage.pdf",
    plot = p,
    width = 6,
    height = 5
  )
  p
}
