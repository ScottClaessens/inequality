#' Plot correlation between linguistic and geographic distances
#'
#' Plot correlation between linguistic distances and geographic distances
#' between D-PLACE societies. Distances are transformed (log + 1) and normalised
#' between 0 and 1 before plotting.
#'
#' @param data Tibble of D-PLACE data
#' @param mcc_tree Maximum clade credibility tree
#'
#' @returns A ggplot object
#'
plot_linguistic_spatial_distance <- function(data, mcc_tree) {
  # get linguistic distance matrix (logged and normalised)
  lin_dist_mat <- log(ape::cophenetic.phylo(mcc_tree) + 1)
  diag(lin_dist_mat) <- 0
  lin_dist_mat <- lin_dist_mat / max(lin_dist_mat)
  # arrange dataset by tip labels
  data <- data[match(mcc_tree$tip.label, data$xd_id), ]
  if (!identical(data$xd_id, mcc_tree$tip.label)) {
    stop("Data and tree are not aligned.")
  }
  # calculate x,y,z coordinates on unit sphere
  data <-
    data |>
    mutate(
      # get latitude and longitude in radians
      xlon = longitude * pi / 180,
      xlat = latitude * pi / 180,
      # convert to x,y,z coordinates
      x = cos(xlat) * cos(xlon),
      y = cos(xlat) * sin(xlon),
      z = sin(xlat)
    )
  # get geographic distance matrix (logged and normalised)
  geo_dist_mat <- log(as.matrix(dist(data[, c("x", "y", "z")])) + 1)
  diag(geo_dist_mat) <- 0
  geo_dist_mat <- geo_dist_mat / max(geo_dist_mat)
  # get correlation between lower triangles
  correlation <- cor(
    lin_dist_mat[lower.tri(lin_dist_mat)],
    geo_dist_mat[lower.tri(geo_dist_mat)]
  )
  # get distances for plot
  distances <-
    tibble(
      linguistic_distance = lin_dist_mat[lower.tri(lin_dist_mat)],
      geographic_distance = geo_dist_mat[lower.tri(geo_dist_mat)]
    )
  # plot distances
  out <-
    ggplot(
      data = distances,
      mapping = aes(
        x = linguistic_distance,
        y = geographic_distance
      )
    ) +
    geom_bin_2d(bins = 50) +
    scale_fill_viridis_c(
      name = "Count",
      option = "A",
      breaks = c(1, 10, 100, 1000, 10000),
      transform = "log"
    ) +
    labs(
      x = "Linguistic distance",
      y = "Geographic distance",
      title = paste0(
        "Correlation between distances: r = ",
        round(correlation, 2)
      ),
      subtitle = "Transformed with log + 1 and normalised between 0-1"
    ) +
    theme_classic()
  # remove datasets to save space
  rm(data, distances, mcc_tree)
  # save
  ggsave(
    plot = out,
    filename = "plots/correlation_distances.pdf",
    height = 4.5,
    width = 5
  )
  # return
  out
}
