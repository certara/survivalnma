#' prep_all_survivals
#'
#' A function that computes the survival function for multiple treatments based on a study
#' of interest
#'
#' @param nmafit a survnma object to be used for subsequent calculations
#' @param study a string of the study of interest
#' @param treatments a string or vector of treatments to be compared
#' @param timesteps Vector of evaluation times. Should be of length > 2
#'
#' @return a dataframe with columns corresponding to
#'         \code{c("curve", "time", "mean", "lci", "uci", "label")}
#'         where curve indicates the family of distribution used for
#'         survnma object and label indicating the corresponding
#'         treatment considered. uci and lci indicate the upper and
#'         lower bounds of the credible interval for the mean.
#'         the dataframe is to be used for plotting via the \code{\link{survival_plot}} function
#'
#' @export

prep_all_survivals <- function(nmafit, study, treatments=nmafit$treatment,
                               timesteps = seq(0,30)) {
  if(class(nmafit) != "survnma"){
    stop("Expected survnma object")
  }
  if(is.null(study))
    stop("No study has been specified, can't calculate survivals.")
  sm <- nmafit$fit$sims.matrix
  mu <- extract_mu(nmafit, study)


  if(length(timesteps) == 1){
    stop("Evaluation time vector should have length > 2")
  }else{
    times <- timesteps
    times[times == 0] <- 0.001
  }


  #labeling work
  treatment.adj <- as.character(nmafit$data$treatment)
  alltreatments <- as.character(nmafit$data$treatment)
  studies <- as.character(nmafit$data$study)
  treatments.instudy <- as.character(treatment.adj[studies == study])
  for( trt in treatment.adj){
    if( !(trt %in% treatments.instudy) ){
  treatment.adj[treatment.adj == trt] <- c(paste0( trt, " (adjusted)"))
    }
  }
  treatment.adj <- unique(treatment.adj)
  treatmentLabel <- treatment.adj[which(alltreatments %in% treatments)]

  res <- data.frame()
  for (i in 1:length(treatments)){
    # d <- extract_d(nmafit, treatments[i])
    d <- relative_d_in_study(nmafit, treatments[i], study)
    params <- mu + d
    surv.points <- survival_curve(nmafit$model, params, times, P= nmafit$P, type = "interval",
                                  warn = TRUE, trt_warn = treatments[i])
    # This warning is now handled inside survival_curve():
    # if(any(is.na(surv.points))){
    #   message("Faulty parameters for ", treatments[i]," caused survival to be NA. ",
    #           "The problematic treatment(s) will be removed.")
    # }
    res <- rbind(res,
                 data.frame(curve = nmafit$model, t = times,
                            mean = surv.points[2,], lci = surv.points[1,],
                            uci = surv.points[3,],
                            # label = treatment.adj[grep(treatments[i], treatment.adj)],
                            label = treatmentLabel[i],
                            stringsAsFactors = FALSE)
    )}
  return(res)
}
