#' hazard_table
#'
#' internal function that generates hazard ratio table at each month
#' if you have pre-calculated hazard values you can pass
#' them directly to reduce computation time.
#'
#' @param survnma survnma object to be passed
#' @param treatments a list of desired treatments to be compared
#' @param reference string of treatment that acts as reference
#' @param timesteps Vector of evaluation times. Should be of length > 2
#' @param hazards precalculated hazards (using \code{\link{prep_all_hazards}})
#'                to reduce computation time
#' @param print Boolean, if true prints results in console, if false stores dataframe
#'
#' @export
#' @importFrom knitr kable


hazard_table <- function(survnma, treatments = NULL, reference,
                         timesteps = seq(0,30),
                         hazards = NULL, print = FALSE){
  # if(!(timesteps %in% c("3month", "monthly"))){
  #   stop("Timesteps should either be `monthly` or `3month`. Change accordingly!")
  # }
  if(is.null(treatments)){
    treatments <- survnma$treatments
  }
  if(is.null(hazards)){
      hazards <- prep_all_hazards(survnma, treatments, reference, timesteps = timesteps)
  }


  hazard <- split(hazards, f = hazards$label)

  treatments <- unique(hazards$label)

  means <- lapply(treatments,
                  function(x) data.frame(hazard[[x]]$mean))
  names(means) <- treatments

  if(length(timesteps) == 1){
    stop("Evaluation time vector should have length > 2")
  }else{
    times <- timesteps
    times[times == 0] <- 0.001
  }

  meansdf <- data.frame(sapply(means,c))
  colnames(meansdf) <- treatments

  meansdf <- cbind(time = times,
                   month = round(times, 0),
                   dt = abs(times - round(times, 0)),
                   meansdf)

  meansdf <- meansdf[, -c(2,3)]

  if(print == TRUE)
    print(knitr::kable(meansdf,
                       align = c(rep('c',ncol(meansdf))),
                       caption = "Hazard Ratio table"))

  else
    return(meansdf)

}
