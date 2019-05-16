#'
#' extract_mu
#'
#' Extracts a matrix of sampled paramaters (mean) via the method of MCMC simulation.
#'
#' @param fit a \code{`survnma`} object to be used for extracting parameters
#' @param study string corresponding to the study of interest
#'
#' @return returns a matrix of extracted parameters
#' @noRd


extract_mu <- function(fit, study) {
  if(class(fit) != "survnma")
    stop("Expected survnma object")
  if(!(study %in% fit$data$study)){
    stop("Study supplied not in the survnma object")
  }
  # studies <- unique(fit$data$study)
  # study.labels <- unique(fit$inputs$s)
  # study.identifier <- study.labels[grep(study, studies)]
  study.identifier <- fit$study_labels[[study]]

  parameter.matrix <- fit$fit$sims.matrix
  if(fit$model == "exponential"){
    extracted.columns.mu <- as.matrix(parameter.matrix[,paste0("mu[", study.identifier, "]")])
  }else{
    extracted.columns.mu <- parameter.matrix[,paste0("mu[", study.identifier, "," , 1:fit$nparam, "]")]
  }

  return(extracted.columns.mu)

}
#'
#' extract_d
#'
#' Extracts a matrix of sampled paramaters (delta) via the method of MCMC simulation.
#'
#' @param fit a \code{`survnma`} object to be used for extracting parameters
#' @param trt string or vector of strings corresponding to the treatment(s) of interest
#'
#' @return returns a matrix of extracted parameters
#' @noRd

extract_d <- function(fit, trt) {
  if(class(fit) != "survnma")
    stop("Expected survnma object")
  if(!(trt %in% fit$treatments)){
    stop("Treatment supplied not in the survnma object")
  }
  # treatments <- unique(fit$data$treatment)
  # treatment.labels <- c(unique(fit$inputs$bs), unique(fit$inputs$ts))
  # treatment.identifier <- treatment.labels[grep(trt, treatments)]
  treatment.identifier <- fit$trt_labels[[trt]]

  # if baseline then d = 0
  if (treatment.identifier == 1){
    extracted.columns.d <- matrix( 0, dim(fit$fit$sims.matrix)[1], fit$nparam)
  }else{
    parameter.matrix <- fit$fit$sims.matrix
    if ( fit$model == "exponential"){
    extracted.columns.d <- parameter.matrix[,paste0("d[", treatment.identifier, "]")]
    }else{
    extracted.columns.d <- parameter.matrix[,paste0("d[", treatment.identifier, "," , 1:fit$nparam, "]")]
    }
  }
  return(extracted.columns.d)
}

#'
#' relative_d_in_study
#'
#' extracts relative d matrix, using transitivity property.
#' If d_ii then d = 0 for all entries
#' Otherwise d_ij = d_1j - d_1i
#'
#'
#' @param nma `survnma` object to be used
#' @param trt a `treatment` to be compared with the baseline treatment of a study
#' @param study a `study` of interest
#' @noRd


relative_d_in_study <- function(nma, trt, study){
  study.baseline <- unique(nma$data$baseline[nma$data$study == study])

  if(trt == study.baseline){
      return( matrix(0, dim(nma$fit$sims.matrix)[1], nma$nparam))
  }else{
    d1 <- extract_d(nma, trt)
    d2 <- extract_d(nma, study.baseline)
    return(d1 - d2)
  }

}
