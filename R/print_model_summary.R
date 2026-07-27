#' Print summary of fitted model
#'
#' Print summary of fitted model and save the result to a text file
#'
#' @param fit Fitted coevfit model
#' @param file_name Output text file
#'
#' @returns Nothing, run for printing side-effect
#'
print_model_summary <- function(fit, file_name) {

  # print model summary
  withr::with_options(
    list(width = 200),
    capture.output(
      suppressWarnings(summary(fit)),
      file = file_name
    )
  )

}
