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
        fill = class_differentiation
      ),
      size = 1.2,
      shape = 21
    ) +
    scale_fill_brewer(
      type = "seq",
      palette = 7,
      guide = guide_legend(
        override.aes = list(
          size = 4,
          shape = "square filled"
        )
      )
    ) +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.key.spacing.y = unit(-2, "mm"),
      legend.position = "inside",
      legend.position.inside = c(0.1, 0.1),
      legend.box.background = element_rect(),
      legend.margin = margin(0, 5, 0, 0)
    )
}
