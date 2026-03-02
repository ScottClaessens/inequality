#' Plot class differentiation variable on a world map
#'
#' Plot the class differentiation variable from the Ethnographic Atlas on a
#' world map
#'
#' @param data Tibble of D-PLACE data
#'
#' @returns A ggplot object
#'
plot_world_map <- function(data) {
  ne_countries(
    scale = "small",
    returnclass = "sf"
  ) |>
    filter(continent != "Antarctica") |>
    ggplot() +
    geom_sf(
      fill = "grey80",
      colour = NA
    ) +
    geom_point(
      data = arrange(
        filter(data, !is.na(class_differentiation)),
        class_differentiation
      ),
      mapping = aes(
        x = longitude,
        y = latitude,
        colour = class_differentiation
      ),
      size = 1
    ) +
    scale_colour_brewer(
      type = "seq",
      palette = 7
    ) +
    guides(
      colour = guide_legend(
        override.aes = list(
          size = 4,
          shape = "square"
        )
      )
    ) +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box.background = element_rect()
    )
}
