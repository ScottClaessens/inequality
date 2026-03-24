#' Manually fix parameters in Stan code
#'
#' Manually fix parameters in Stan code produced by the coevolve package in
#' order to generate synthetic data from a particular causal model.
#'
#' @param stan_code String. Stan code produced by the coevolve package.
#' @param parameters List of parameters values to fix, including "intercept",
#'   "auto_effect", "cross_effect", "drift", and "init"
#' @param model String of length one. Causal model used to determine which
#'   parameters to fix in Stan code. Must be equal to one of the following:
#'   "agriculture", "intergenerational_wealth_transmission", "family",
#'   "population_size", "land_limited", "scalar_stress", "intergroup_conflict",
#'   "bridewealth", "craft_specialisation", or "food_storage"
#'
#' @returns Stan code with fixed parameters as a string
#'
fix_parameters <- function(stan_code, parameters, model) {
  # get effects matrix for model
  effects_matrix <- get_effects_matrix(model)
  # get list of ordinal cut points for model
  cutpoints <- get_cutpoints_list(model)
  # construct strings for adding and removing ordinal cut points
  add_cutpoints <- ""
  remove_cutpoints <- ""
  for (i in 1:length(cutpoints)) {
    if (!is.null(cutpoints[[i]])) {
      add_cutpoints <-
        paste0(
          add_cutpoints,
          "  ordered[", length(cutpoints[[i]]), "] c", i, " = ["
        )
      for (j in 1:length(cutpoints[[i]])) {
        add_cutpoints <- paste0(
          add_cutpoints, cutpoints[[i]][j],
          ifelse(j == length(cutpoints[[i]]), "", ", ")
        )
      }
      add_cutpoints <- paste0(add_cutpoints, "]';\n")
      remove_cutpoints <-
        paste0(
          remove_cutpoints,
          "  ordered[", length(cutpoints[[i]]), "] c", i, "; ",
          "// cut points for variable ", i, "\n"
        )
    }
  }
  # construct stan code for setting A off-diagonals
  set_offdiag <- ""
  for (i in 1:nrow(effects_matrix)) {
    for (j in 1:ncol(effects_matrix)) {
      if (i != j) {
        if (effects_matrix[i, j]) {
          set_offdiag <-
            paste0(
              set_offdiag,
              "  A[", i, ",", j, "] = ", parameters$cross_effect, ";\n"
            )
        }
      }
    }
  }
  # edit stan code to manually fix parameters
  stan_code <-
    stan_code |>
    str_remove(
      fixed(
        paste0(
          "  vector<upper=0>[J] A_diag; // autoregressive terms of A\n",
          "  vector[num_effects - J] A_offdiag; // cross-lagged terms of A\n",
          "  vector<lower=0>[J] Q_sigma; // std deviation parameters of the ",
          "Q mat\n",
          "  vector[J] b; // SDE intercepts\n",
          "  array[N_tree] vector[J] eta_anc; // ancestral states\n"
        )
      )
    ) |>
    str_remove(fixed(remove_cutpoints)) |>
    str_replace(
      pattern = fixed(
        paste0(
          "  matrix[J,J] A = diag_matrix(A_diag); // selection matrix\n",
          "  matrix[J,J] Q = diag_matrix(Q_sigma^2); // drift matrix\n"
        )
      ),
      replacement = paste0(
        "  array[N_tree] vector[J] eta_anc;\n",
        "  vector[J] b = rep_vector(", parameters$intercept, ", J);\n",
        "  matrix[J,J] A = diag_matrix(rep_vector(", parameters$auto_effect,
        ", J));\n",
        "  matrix[J,J] Q = diag_matrix(rep_vector(", parameters$drift,
        ", J));\n",
        add_cutpoints
      )
    ) |>
    str_replace(
      pattern = fixed(
        paste0(
          "  // fill off diagonal of A matrix\n",
          "  {\n",
          "    int ticker = 1;\n",
          "    for (i in 1:J) {\n",
          "      for (j in 1:J) {\n",
          "        if (i != j) {\n",
          "          if (effects_mat[i,j] == 1) {\n",
          "            A[i,j] = A_offdiag[ticker];\n",
          "            ticker += 1;\n",
          "          } else if (effects_mat[i,j] == 0) {\n",
          "            A[i,j] = 0;\n",
          "          }\n",
          "        }\n",
          "      }\n",
          "    }\n",
          "  }\n"
        )
      ),
      replacement = paste0(
        set_offdiag,
        "  for (t in 1:N_tree) eta_anc[t] = rep_vector(", parameters$init,
        ", J);\n"
      )
    ) |>
    str_remove(fixed("  b ~ std_normal();\n")) |>
    str_remove(fixed("    eta_anc[t] ~ std_normal();\n")) |>
    str_remove(fixed("  A_offdiag ~ std_normal();\n")) |>
    str_remove(fixed("  A_diag ~ std_normal();\n")) |>
    str_remove(fixed("  Q_sigma ~ std_normal();\n"))
  # remove remaining cutpoint priors
  for (i in 1:length(cutpoints)) {
    if (!is.null(cutpoints[[i]])) {
      stan_code <-
        str_remove(
          stan_code,
          fixed(
            paste0("  c", i, " ~ normal(0, 2);\n")
          )
        )
    }
  }
  # return
  stan_code
}
