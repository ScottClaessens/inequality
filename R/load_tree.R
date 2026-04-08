#' Load global language phylogeny
#'
#' Load the posterior treeset for the global language phylogeny from a Nexus
#' file and edit the tree samples such that all branches have positive non-zero
#' lengths.
#'
#' @param tree_file Path to global language tree Nexus file
#'
#' @returns A multiPhylo object
#'
load_tree <- function(tree_file) {
  # load nexus file
  tree <- read.nexus(tree_file)
  # manually fix branch lengths with very small lengths (for modelling)
  for (t in 1:length(tree)) {
    for (i in which(tree[[t]]$edge.length < 0.001)) {
      tree[[t]]$edge.length[i] <- 0.001
    }
  }
  # return
  tree
}
