#'
#' generate_hazard
#'
#' internal function to generate the hazard function based on input distribution family
#'
#' @param family family of distribution \code{c("weibull", "gompertz",
#'                                              "exponential", "log-normal",
#'                                              "log-logistic", "fp1", "fp2")}
#' @param params matrix of parameters
#'
#' @param times sequence of times
#'
#' @param P a vector of powers (only required for fp1, fp2)
#' \itemize{
#'   \item{if family is fp1 then only supply one integer for p1}
#'   \item{if family is fp2 then supply a vector with two digits. ie c(1,-1)}}
#'
#'
#' @return matrix of hazard evaluated multiple points
#' @noRd
#'

generate_hazard <- function(family, params, times, P=NULL){
  if (family == "weibull") {
    hazard <- exp(params[1] + params[2]*log(times))

  }else if (family == "gompertz") {
    hazard <- exp(params[1])* exp(times*(params[2]))
  } else if (family == "exponential") {
    hazard <- exp(params[1])
  } else if (family == "lognormal") {
    numerator <- dnorm( (log(times) - params[1]) / exp(params[2]) )
    denominator <- (exp(params[2])*times)*(1 - pnorm( (log(times) - params[1])/ exp(params[2])))
    hazard <- numerator/denominator

  } else if (family == "loglogistic") {
    hazard <- (exp(params[2])/exp(params[1]))*((times/exp(params[1]))^(exp(params[2])-1)) /
              (1+((times/exp(params[1])) ^ exp(params[2])))
  } else if (family == "fp1") {
    if (is.null(P))                    {stop("Power vector cannot be NULL")
    }else if (length(P) > 1)           {stop("Power vector needs to be of length 1")
    }else if (P==0)                    {hazard <- exp(params[1] + params[2]*log(times))
    }else                              {hazard <- exp(params[1] + params[2]*times^P)
    }

  } else if (family == "fp2") {
    if (is.null(P))                    {stop("Power vector needs to be of length 2")
    }else if(length(P) == 1)           {stop("Power vector needs to be of length 2")
    }else if(length(P) > 2)            {stop("Power vector needs to be of length 2")
    }else if(P[1] ==0 && P[2] ==0)     {hazard <- exp(params[1] + params[2]*log(times)   + params[3]*(log(times))^2)
    }else if(P[1] != 0 && P[2] == 0)   {hazard <- exp(params[1] + params[2]*times^P[1]   + params[3]*(log(times)))
    }else if(P[1] == 0 && P[2] !=0)    {hazard <- exp(params[1] + params[2]*log(times)   + params[3]*times^P[2])
    }else if(P[1] == P[2] && P[1] !=0) {hazard <- exp(params[1] + params[2]*times^P[1]   + params[3]*(times^P[1])*log(times))
    }else                              {hazard <- exp(params[1] + params[2]*times^P[1]   + params[3]*times^P[2])
    }

  } else {
    stop("Wrong or unspecified family of distributions")
  }
  # setting a threshold if hazard values get really large
  hazard[hazard > exp(100)] <- exp(100)
  return(hazard)
}
