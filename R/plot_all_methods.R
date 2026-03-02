#' Combine all methods plots into one figure
#'
#' Combine subplots on (a) the global distribution of the class differentiation
#' variable, (b) the maximum clade credibility tree, and (c) the tree traversal
#' algorithm for generalised dynamic phylogenetic models.
#'
#' @param plot_world A ggplot object. World map of the class variable.
#' @param plot_tree A ggplot object. Circular plot of the global language tree.
#' @param plot_world A ggplot object. Schematic of the tree traversal algorithm
#'   for generalised dynamic phylogenetic models.
#'
#' @returns A ggplot object
#'
plot_all_methods <- function(plot_world, plot_tree, plot_gdpm) {
  # bottom row
  bottom <- plot_grid(
    plot_tree,
    plot_gdpm,
    nrow = 1,
    labels = c("B", "C")
  )
  # combine all
  p <- plot_grid(
    plot_world,
    bottom,
    nrow = 2,
    labels = c("A", "")
  )
  # save
  ggsave(
    filename = "plots/methods.pdf",
    plot = p,
    height = 9,
    width = 9
  )
  p
}
