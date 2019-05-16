#' prep_all_hazards
#'
#' A function that computes the hazard function and thus
#' hazard ratio for multiple treatments based on a study
#' of interest
#'
#' @param nmafit a \code{survnma} object to be used for subsequent calculations
#' @param treatments a string or vector of treatments to be compared
#' @param reference a base treatment to be used for comparison when calculating
#'        the hazard ratio
#' @param timesteps Vector of evaluation times. Should be of length > 2
#'
#' @return a dataframe with columns corresponding to
#'         \code{c("curve", "time", "mean", "lci", "uci", "label")}
#'         where curve indicates the family of distribution used for
#'         survnma object and label indicating the corresponding
#'         treatment considered. uci and lci indicate the upper and
#'         lower bounds of the credible interval for the mean.
#'         the dataframe is to be used for plotting via the \code{\link{hazard_plot}} function
#'
#' @export


prep_all_hazards <- function(nmafit, treatments = NULL, reference,
                             timesteps = seq(0,30)) {
  if( class(nmafit) != "survnma"){
    stop("Expected survnma object")
  }
  if(is.null(treatments)){
    treatments <- nmafit$treatments
  }
  sm <- nmafit$fit$sims.matrix

  if(length(timesteps) == 1){
    stop("Evaluation time vector should have length > 2")
  }else{
    times <- timesteps
    times[times == 0] <- 0.001
  }


  # reference work
  params.ref <- extract_d(nmafit, reference)
  hazard.ref <- hr_curve(nmafit$model, params.ref, times, P = nmafit$P)

  # collect hazard of non-reference treatments
  # comparable.treats <- treatments[treatments != reference]
  comparable.treats <- treatments
  res <- data.frame()
  for (i in 1:length(comparable.treats)){
    d <- extract_d(nmafit, comparable.treats[i])
    params <- d
    hazard.treat <- hr_curve(nmafit$model, params, times, P=nmafit$P)
    hazard.ratio <- hazard.treat/hazard.ref

    res <- rbind(res,
                 data.frame(curve = nmafit$model, t= times,
                            mean = hazard.ratio[2,], lci = hazard.ratio[1,],
                            uci = hazard.ratio[3, ], label = comparable.treats[i],
                            ref = reference))
  }

  res$label <- as.character(res$label)
  res$label[grepl(reference, res$label)] <- as.character(paste0(reference, " (reference)"))
  # res <- filter(res, t != 0)
  return(res)
}
