#' Extract posterior draws for standardised selection matrix from fitted model
#'
#' @param fit Fitted coevfit model
#'
#' @returns Three-dimensional array, N samples x N variables x N variables
#'
extract_standardised <- function(fit) {

  # extract posterior samples
  post <- extract_samples(fit)

  # get posterior selection matrix (unstandardised)
  A <- post$A

  # get posterior estimates for tip trait values
  eta_tips <- post$eta[, , 1:1258, ]

  # average tip trait values across trees
  eta_tips <- apply(eta_tips, c(1, 3, 4), mean)

  # get standard deviations across tips
  eta_sd <- apply(eta_tips, c(1, 3), sd)

  # standardise selection matrix
  N_vars <- length(fit$variables)
  A_std <- A
  for (i in 1:N_vars) {
    for (j in 1:N_vars) {
      A_std[, i, j] <- A[, i, j] * (eta_sd[, j] / eta_sd[, i])
    }
  }

  # return
  A_std

}
