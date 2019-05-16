#' Print out survnma object
#' nothing fancy for now, S3 method
#' to stop from printing all of survnma
#' object with its inputs etc. etc.
#' @param x Object of class `survnma`
#' @param ... additional arguments to print function
#' @export
print.survnma <- function(x, ...) {
  cat(paste0("survival NMA with ", x$type, " effects, ", x$model, " family of distributions\n"))
  cat("------------------------------------------------------------\n")
  cat(paste0("Treatments (d): ",
             paste0(names(x$trt_labels), ": ", x$trt_labels, collapse = ", "), "\n"))
  cat(paste0("Studies (mu): ",
             paste0(names(x$study_labels), ": ", x$study_labels, collapse = ", "), "\n"))
  cat("------------------------------------------------------------\n")
  print(x$fit)
  invisible(x)
}
