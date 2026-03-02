#' Plot a diagram of generalised dynamic phylogenetic models (GDPMs)
#'
#' Plot a schematic diagram outlining the tree traversal algorithm underlying
#' generalised dynamic phylogenetic models (GDPMs)
#'
#' @returns A ggplot object
#'
plot_gdpm_algorithm <- function() {
  # set random seed
  set.seed(123)
  # top row: co-evolving latent variables
  # set up simulation
  n <- 60
  d <- data.frame(
    t = 1:n,
    x = c(0.1, rep(NA, n - 1)),
    y = c(-0.1, rep(NA, n - 1))
  )
  for (t in 2:n) {
    # autoregressive effects
    d$x[t] <- d$x[t - 1] * 0.9
    d$y[t] <- d$y[t - 1] * 0.9
    # cross-selection effect x -> y
    d$y[t] <- d$y[t] + d$x[t - 1] * 0.05
    # random drift
    d$x[t] <- d$x[t] + rnorm(1, 0, 0.15)
    d$y[t] <- d$y[t] + rnorm(1, 0, 0.15)
  }
  # pivot simulation data longer
  d <- pivot_longer(
    data = d,
    cols = x:y
  ) |>
    mutate(name = ifelse(name == "x", "Variable 1", "Variable 2"))
  # plot top
  top <-
    ggplot(
      data = d,
      mapping = aes(
        x = t,
        y = value,
        colour = name
      )
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(
      data = d[d$t == 1 | d$t == n, ],
      mapping = aes(
        x = t,
        y = value,
        colour = name
      ),
      size = 2,
      show.legend = FALSE
    ) +
    labs(
      x = "Time along branch",
      y = "Latent trait value"
    ) +
    scale_colour_manual(values = c("purple", "darkseagreen3")) +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.11, 0.85),
      legend.title = element_blank(),
      legend.key.spacing.y = unit(-2.5, "mm"),
      panel.background = element_rect(fill = "#faf3fe"),
      legend.background = element_rect(fill = "#faf3fe"),
      legend.margin = margin()
    )
  # bottom row: example phylogenetic tree
  # simulate tree
  tree <- ape::rcoal(6)
  tree$tip.label <- c("F", "E", "B", "A", "C", "D")
  # plot bottom
  bottom <-
    ggtree(
      tr = tree,
      linewidth = 1
    )
  # add highlights
  bottom <-
    bottom +
    geom_segment2(
      data = subset(bottom@data, node == 10),
      x = 0.296,
      y = 4.75,
      xend = 0.564,
      yend = 4.75,
      linewidth = 5,
      alpha = 0.2,
      colour = "purple"
    ) +
    annotate(
      x = 0.43,
      y = 5.5,
      xend = 0.43,
      yend = 7,
      arrow = arrow(
        length = unit(2, "mm"),
        type = "closed"
      ),
      linewidth = 1,
      colour = "purple",
      geom = "segment"
    )
  # rest of the plot
  bottom <-
    bottom +
    geom_tippoint(
      colour = "black",
      size = 3
    ) +
    geom_nodepoint(
      colour = "black",
      fill = "white",
      size = 3,
      shape = "circle filled"
    ) +
    geom_tiplab(hjust = -0.6) +
    scale_x_continuous(name = "Time") +
    ylim(c(0.5, 7)) +
    theme(
      axis.line.x = element_line(
        arrow = arrow(
          length = unit(2, "mm"),
          type = "closed"
        )
      ),
      axis.text.x = element_blank()
    )
  # put together
  plot_grid(
    NULL,
    top,
    bottom,
    nrow = 3,
    rel_heights = c(0.15, 0.85, 1)
  )
}
