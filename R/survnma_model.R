
#' survnma_model
#'
#' Show the WinBUGS model used by survnma in a separate window.
#'
#' @param model one of `fp1`, `fp2`, `weibull`, `exponential`, `loglogistic`, `lognormal`, `gompertz`
#' @param type `fixed` or `random`
#'
#' @export

survnma_model <- function(model, type) {
  # grab the included WinBUGS model, write a temporary file
  file <- system.file("bugs_models",
                      model_filenames[[type]][[model]],
                      package = "survnma")
  file.show(file)
}
