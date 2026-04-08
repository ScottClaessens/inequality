#' Plot prior for spatial Gaussian process kernel function
#'
#' @returns A ggplot object
#'
plot_spatial_gp_prior <- function() {
  withr::with_seed(1234, {
    # vector of distances between 0 and 1
    n <- 100
    distance <- seq(0, 1, length.out = n)
    # simulate draws from the prior for gaussian process kernel function
    ndraws <- 50
    sdgp <- rexp(ndraws, 2) # sdgp ~ exponential(2)
    rho  <- rexp(ndraws, 6) # rho  ~ exponential(6)
    covariance <- matrix(nrow = ndraws, ncol = n)
    for (i in 1:n) {
      covariance[, i] <- sdgp^2 * exp(-(1 / (2 * rho^2)) * distance[i]^2)
    }
    colnames(covariance) <- distance
    # plot
    out <-
      covariance |>
      as_tibble() |>
      pivot_longer(
        cols = everything(),
        names_to = "distance",
        values_to = "covariance"
      ) |>
      mutate(
        draw = rep(1:ndraws, each = n),
        distance = as.numeric(distance)
      ) |>
      ggplot(
        mapping = aes(
          x = distance,
          y = covariance,
          group = draw
        )
      ) +
      geom_line() +
      labs(
        x = "Geographic distance (normalised)",
        y = "Covariance"
      ) +
      theme_classic()
  })
  # save and return
  ggsave(
    plot = out,
    filename = "plots/prior_spatial_gp.pdf",
    height = 4,
    width = 4
  )
  out
}
