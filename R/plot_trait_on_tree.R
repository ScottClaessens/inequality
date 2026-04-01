#' Plot trait on maximum clade credibility tree
#'
#' @param data Tibble of D-PLACE data
#' @param mcc_tree Maximum clade credibility tree of D-PLACE societies
#'
#' @returns A ggplot object
#'
plot_trait_on_tree <- function(data, mcc_tree, variable) {
  # wrangle data
  d <-
    data |>
    transmute(var = as.factor(as.numeric(!!sym(variable)))) |>
    as.data.frame()
  rownames(d) <- data$xd_id
  # plot tree
  tree <-
    ggtree(
      tr = mcc_tree,
      layout = "circular",
      linewidth = 0.1
    )
  # add data to tree
  out <-
    gheatmap(
      p = tree,
      data = d,
      offset = -0.5,
      width = 0.05,
      colnames = FALSE,
      color = NA
    )
  if (is.ordered(data[[variable]])) {
    out <-
      out +
      scale_fill_brewer(
        name = str_to_sentence(str_replace_all(variable, "_", " ")),
        labels = function(x) levels(data[[variable]])[as.numeric(x)],
        type = "seq",
        palette = 7,
        na.value = "grey95"
      )
  } else {
    out <-
      out +
      scale_fill_manual(
        name = str_to_sentence(str_replace_all(variable, "_", " ")),
        labels = function(x) c("Absent", "Present")[as.numeric(x)],
        values = c("#ADD8E6", "#26667C")
      )
  }
  # save
  ggsave(
    filename = paste0("plots/tree/tree_", variable, ".pdf"),
    plot = out,
    height = 10,
    width = 10
  )
  # return
  out
}
