#' Read many digitized Kaplan-Meier curves
#'
#' The function reads a collection of files containing data from digitized Kaplan-Meier curves.
#'
#' @param path path to a folder containing txt files from digitizing; each KM file
#'             should have 4 columns in the following order: time, number at risk,
#'             number censored, number of events
#' @param preferred_baseline character; if you want to specify a preferred baseline treatment
#' @param filter character; only files containing \code{filter} in title will be used
#' @param split character; sign that separates study, outcome and treatment
#' @param study integer explaining naming convention, see below
#' @param outcome integer explaining naming convention, see below
#' @param treatment integer explaining naming convention, see below
#' @details
#' By default we assume that the files will follow naming convention such as
#' \code{study_outcome_trt_KM.txt}, e.g. \code{MOT_OS_Pazo_KM.txt}. If the naming
#' scheme is different, user can reorder at will or change the separation sign, e.g.
#' for \code{OS-Pazo-MOT-KM.txt} we would say
#' \code{split = "-", study = 3, outcome = 1, treatment = 2}.
#' @return
#' Data frame which can be used as input into NMA models using \code{\link{survnma}}.
#' @export

read_km_folder <- function(path,
                           preferred_baseline = NULL,
                           filter = "KM.txt",
                           split = "_",
                           study = 1,
                           outcome = 2,
                           treatment = 3
                           ) {
  lf <- list.files(path)
  lf <- lf[grepl(filter, lf)]
  df <- do.call(rbind,
                lapply(as.list(lf), function(x) {
                  y <- strsplit(x, split)[[1]]
                  data.frame(
                    study = tolower(y[1]),
                    outcome = tolower(y[2]),
                    treatment = tolower(y[3]),
                    filepath = paste0(path,x),
                    stringsAsFactors = FALSE)
                }))

  # df <- add_baseline_column(df, preferred_baseline)

  return(df)
}

