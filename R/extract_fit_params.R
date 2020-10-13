
#' extract_mu
#'
#' Extract a posterior sample of study baseline parameters in a `survnma`.
#'
#' @param fit a \code{`survnma`} object to be used for extracting parameters
#' @param study string corresponding to the study of interest
#'
#' @return returns a matrix of extracted parameters
#' @export
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



#' extract_d
#'
#' Extract a posterior sample of treatment effect parameters in a `survnma`.
#'
#' @param fit a [survnma] object to be used for extracting parameters
#' @param trt a string or character vector corresponding to the treatment(s) of interest
#'
#' @return a matrix of extracted parameters (columns are parameters, rows are MCMC samples)
#' @export

extract_d <- function(fit, trt) {
  if(class(fit) != "survnma")
    stop("Expected survnma object")
  if(!(trt %in% fit$treatments)){
    stop("Treatment supplied not in the survnma object")
  }
  treatment.identifier <- fit$trt_labels[[trt]]

  # if baseline then d = 0
  if (treatment.identifier == 1){
    extracted.columns.d <- matrix( 0, dim(fit$fit$sims.matrix)[1], fit$nparam)
  }else{
    parameter.matrix <- fit$fit$sims.matrix
    if ( fit$model == "exponential")
      extracted.columns.d <- parameter.matrix[,paste0("d[", treatment.identifier, "]")]
    else
      extracted.columns.d <- parameter.matrix[,paste0("d[", treatment.identifier, "," , 1:fit$nparam, "]")]
  }
  return(extracted.columns.d)
}



#' relative_d_in_study
#'
#' Calculate effect of a given treatment if it was used in a given study.
#'
#' This function extracts relative `d_ij` matrix, by finding the baseline treatment
#' and calculating difference in treatment effect from that baseline.
#' If `i=j`, then d = 0 for all entries. Otherwise `d_ij = d_1j - d_1i`.
#'
#' @param nma `survnma` object to be used
#' @param trt a `treatment` to be compared with the baseline treatment of a study
#' @param study a `study` of interest
#' @export

relative_d_in_study <- function(nma, trt, study){
  study.baseline <- unique(nma$data$baseline[nma$data$study == study])
  # igraph::get.shortest.paths(out$graph, from = "suni", to = "tivo")

  if(trt == study.baseline){
      return( matrix(0, dim(nma$fit$sims.matrix)[1], nma$nparam))
  }else{
    d1 <- extract_d(nma, trt)
    d2 <- extract_d(nma, study.baseline)
    return(d1 - d2)
  }

}
