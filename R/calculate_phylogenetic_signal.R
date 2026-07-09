#' Calculate phylogenetic signal
#'
#' Calculate phylogenetic signal for a particular variable by fitting a Bayesian
#' phylogenetic multilevel model and extracting the results
#'
#' @param data Tibble of D-PLACE data
#' @param model Compiled Stan model
#' @param tree Posterior treeset of D-PLACE societies
#' @param variable Variable to use as the outcome in the model
#' @param tree_ids Indices of posterior phylogenetic tree samples to use
#'
#' @returns A tibble of results
#'
calculate_phylogenetic_signal <- function(data, model, tree, variable,
                                          tree_ids) {

  # which rows have observed data?
  obs_idx <- which(!is.na(data[[variable]]))

  # get observed data for outcome variable
  y <- as.numeric(data[[variable]][obs_idx])

  # get cholesky decomposition of phylogenetic correlation matrix
  list_L_cor_phy <- list()
  for (i in 1:length(tree_ids)) {
    cor_phy <- ape::vcv.phylo(tree[[tree_ids[i]]], corr = TRUE)
    cor_phy <- cor_phy[data$xd_id, data$xd_id]
    L_cor_phy <- t(chol(cor_phy[obs_idx, obs_idx]))
    list_L_cor_phy[[i]] <- L_cor_phy
  }

  # get longitude / latitude coordinates in radians
  xlon <- data$longitude * pi / 180
  xlat <- data$latitude  * pi / 180

  # get x,y,z coordinates on a unit sphere
  coords <- matrix(nrow = length(xlon), ncol = 3)
  coords[, 1] <- cos(xlat) * cos(xlon)
  coords[, 2] <- cos(xlat) * sin(xlon)
  coords[, 3] <- sin(xlat)

  # normalise x,y,z coordinates so that maximum distance = 1
  coords <- coords / max(stats::dist(coords))

  # get cholesky decomposition of geographic correlation matrix
  # using exponentiated quadratic covariance kernel with lscale = 0.1
  dist_geo <- as.matrix(dist(coords))
  lscale <- 0.1
  cor_geo <- exp(-(1 / (2 * lscale^2)) * dist_geo^2)
  cor_geo <- cor_geo + diag(1e-7, nrow(cor_geo))
  L_cor_geo <- t(chol(cor_geo[obs_idx, obs_idx]))

  # construct data list for stan
  data_list <-
    list(
      N = length(y),
      P = length(tree_ids),
      K = length(unique(y)),
      y = y,
      L_cor_phy = list_L_cor_phy,
      L_cor_geo = L_cor_geo
    )

  # fit model
  fit <-
    model$sample(
      data = data_list,
      parallel_chains = 4,
      iter_warmup = 2000,
      iter_sampling = 2000,
      seed = 1
    )

  # get posterior draws
  draws <- fit$draws(c("sigma_phy", "sigma_geo"))

  # diagnostics
  diagnostics <-
    fit$summary(
      c("sigma_phy", "sigma_geo"),
      c("rhat", "ess_bulk", "ess_tail")
    )

  # calculate signal (residual variance = pi^2 / 3 for ordered logistic models)
  phy_var <- as.vector(draws[, , "sigma_phy"]^2)
  geo_var <- as.vector(draws[, , "sigma_geo"]^2)
  res_var <- pi^2 / 3
  total_var <- phy_var + geo_var + res_var
  phy_signal <- phy_var / total_var
  geo_signal <- geo_var / total_var

  # return result as tibble
  tibble(
    variable            = variable,
    signal_type         = c("phylogenetic", "geographic"),
    signal              = c(list(phy_signal), list(geo_signal)),
    rhat                = diagnostics$rhat,
    ess_bulk            = diagnostics$ess_bulk,
    ess_tail            = diagnostics$ess_tail,
    num_divergences     = sum(fit$diagnostic_summary()$num_divergent)
  )

}
