#' Survival network meta-analysis models
#'
#' survnma provides interface for Bayesian NMA WinBUGS models
#' (specifically through `R2WinBUGS`). It generates samples
#' from the posterior distribution and interprets them in terms of survivals and hazards.
#'
#' @param nma_df a \code{data.frame} with columns `treatment`, `study`, `baseline` and `filepath`;
#'               each `filepath` should be a path to a text file corresponding to a Kaplan-Meier curve;
#'               each file should have 3 columns, in the following order: time, number at risk,
#'               number of events. Names of columns do not matter.
#'               Refer to [read_km_folder] for automatically creating inputs from folders of text files.
#' @param model \code{string}; family of survival curves to use
#'              Use one of: `weibull`, `gompertz`, `exponential`
#'              `lognormal`, `loglogistic`, `fp1`, `fp2`.
#'              The last two refer to fractional polynomials of order one and two.
#' @param type a \code{string} of characters indicated whether you are considering
#'        "fixed" or "random" effects when modelling the data
#' @param P \code{numeric} or \code{vector} of numbers indicating the powers to
#'        be used when considering fractional polynomials (can be \code{NULL})
#' @param prior a list of prior values, with elements `mean`, `prec2` and `R` to be passed to WinBUGS;
#'              if `prior=NULL`, the defaults are used;
#'              dimensionality of values must be same as of the model
#'              (1 for exponential, 3 for fp2, 2 for the rest);
#'              `mean` is mean treatment effect (TE), default is 0;
#'              `prec2` is precision of TE, default is 0.001*Identity
#'              `R` is scale matrix of Wishart, only used in random effects model, default is Identity;
#'              if using informative priors, we recommend setting `inits=NULL` or generating by hand
#' @param inits if \code{inits="generate"}, initial values will be generated
#'              automatically within R, using Normal(0, 1) for d1/mu1 and Normal(0, 0.1^2)
#'              for d2/mu2 values;
#'              if \code{inits=NULL}, they will be generated automatically by WinBUGS
#'              and much more variable than when generated within R;
#'              if list is supplied, it will be used in a standard WinBUGS way
#'              (see \code{\link[R2WinBUGS]{bugs}})
#' @param n.chains number of chains for WinBUGS to use, default is 3
#' @param auto_restart Boolean to continue running model until convergence or
#'        stop after 5 iterations
#' @param n.iter integer specifying how many interations to be ran
#' @param warnings logical; if FALSE, warning messages about lack of convergence are skipped
#' @param connected_check logical; if FALSE, does not check if network is connected
#' @param min_time_change numeric; a threshold for merging data points where differences in time are
#'                        very small (see Details). We suggest a range of 0.01-0.05.
#'                        Default of 0 does not modify any data.
#' @param bugs.directory WinBUGS directory path; defaults to `C:/WinBUGS14`
#' @param ... additional parameters passed to \code{\link[R2WinBUGS]{bugs}}
#'            (e.g. \code{inits}, \code{n.iter})
#'
#' @return a `survnma` class object
#'
#' @details
#' WinBUGS implementation is through `R2WinBUGS` package. Additional arguments can be passed via `...`,
#' including `bugs.directory`. We use default of `C:/WinBUGS14/`.
#'
#' WinBUGS models code follows publications of M.J. Ouwens and J.P. Jansen (see References).
#' Models can be viewed with \code{\link[survnma]{survnma_model}}.
#'
#' Input data have to follow a particular format, typically obtained from [read_km_folder] but
#' `data.frame` of inputs can also be specified manually.
#'
#' Both initial values and priors can be specified by hand. To understand how they are used,
#' it is recommended to inspect the model with \code{\link[survnma]{survnma_model}} first. The
#' default generation of initial values (`inits = "generate"`) is optimised toward default
#' vague priors. When using informative priors it is best to set `inits = NULL`.
#'
#' @references
#' (1) Ouwens, Mario J. N. M., Zoe Philips, and Jeroen P. Jansen.
#' “Network Meta-Analysis of Parametric Survival Curves.”
#' Research Synthesis Methods 1, no. 3–4 (July 2010): 258–71. https://doi.org/10.1002/jrsm.25.
#'
#' (2) Jansen, Jeroen P. “Network Meta-Analysis of Survival Data with Fractional Polynomials.”
#' BMC Medical Research Methodology 11, no. 1 (May 6, 2011): 61. https://doi.org/10.1186/1471-2288-11-61.
#'
#' @author Witold Wiecek, Savvas Pafitis
#' @seealso `vignette(survivalnma_guide)` for overview,
#'         for working with outputs \code{\link[survnma]{survival_plot}}
#'         and \code{\link[survnma]{hazard_plot}}
#'
#' @examples
#' # We read data from some included data files:
#' nma_df <- data.frame(
#'              stringsAsFactors = FALSE,
#'              "treatment" = c("Suni", "Ifn", "Suni", "Pazo"),
#'              "study" = c("Study 1", "Study 1", "Study 2", "Study 2"),
#'              "baseline" = c("Suni", "Suni", "Suni", "Suni"),
#'              "filepath" = sapply(c("Mota_OS_Suni_KM.txt",
#'                                    "Mota_OS_Ifn_KM.txt",
#'                                    "Mot_OS_Suni_KM.txt",
#'                                    "Mot_OS_Pazo_KM.txt"), function(x)
#'               system.file("extdata", "narrow", x, 
#'                           package="survivalnma", mustWork=TRUE)))
#'
#' # example of simple survnma models (fixed effects)
#' fit_wbl <- survnma(nma_df, "weibull", min_time_change = 0.05)
#' fit_fp2 <- survnma(nma_df, "fp2", P = c(-1,0), min_time_change = 0.05)
#'
#' #manually setting a (very informative) prior
#' #dimension of prior is 2 as Weibull is 2-parameter dist.
#' fit_wbl <- survnma(nma_df, "weibull",
#'                    prior = list("mean" = c(1,1), "prec2" = .1*diag(2)),
#'                    min_time_change = 0.05)
#'
#' #manually setting some WinBUGS options
#' \donttest{survnma(nma_df, "weibull", n.iter = 10000, debug = TRUE)}
#'
#' @importFrom R2WinBUGS bugs
#' @importFrom utils read.table
#' @export
#'

# n.chains=3, n.iter=ni, n.burnin=nb, n.thin=nt
survnma <- function(nma_df = NULL,
                    model = "exponential",
                    # model_path,
                    type = "fixed",
                    prior = NULL,
                    P=NULL,
                    inits="generate",
                    n.chains=3,
                    n.iter = 5000,
                    auto_restart = FALSE,
                    connected_check = TRUE,
                    warnings = TRUE,
                    bugs.directory = "C:/WinBUGS14",
                    min_time_change = 0,
                    ...) {
  # if(exists(deparse(substitute(nma_df)), parent.frame()) == FALSE)
  # stop("Supply dataframe!")

  if((!("data.frame" %in% class(nma_df))) || (nrow(nma_df) == 0))
    stop("Input data nma_df must be a (non-empty) data.frame object, see ?survnma")

  if(!(type %in% c("fixed", "random")))
    stop("'type' has to be 'fixed' or 'random'")

  if(connected_check == TRUE)
    if(check_connected(nma_df) == FALSE)
      stop("Network is not connected. Check baselines")

  studies <- unique(nma_df$study)
  counts <- lapply(studies, function(study) sum(nma_df$study == study))
  if(any(counts < 2)){
    stop("Dataframe contains one-arm studies. Please check studies used")
  }
  if(!(model %in% c("weibull", "gompertz", "exponential", "loglogistic", "lognormal", "fp1", "fp2"))){
    stop(paste("Unrecognized model family.",
               "Available models: weibull, gompertz, exponential, loglogistic, lognormal, fp1, fp2"))
  }
  # prepare model (by writing into working dir...)
  # prep_model_file(model, type)
  model_path <- prep_model_file(model, type)

  # ---FOR NOW HR FEATURE IS DISABLED BECAUSE IT'S---
  # ---NOT CONSISTENT WITH THE OTHER MODELS       ---
  # Conduct a simple HR analysis if possible/required
  # Return HR analysis without going any further
  # if(model == "hr") {
  # fit <- hr_nma(nma_df, prior, n.chains, n.iter, ...)
  # return(fit)
  # out <- structure(list(
  #   fit = fit,
  #   treatments = unique(nma_df$treatment),
  #   trt_labels = attr(inputs, "trt_labels"),
  #   study_labels = attr(inputs, "study_labels"),
  #   inputs = inputs,
  #   model = model,
  #   type = type,
  #   tmax = max(inputs$time),
  #   nparam = as.numeric(dim),
  #   P = P,
  #   data = nma_df))
  # class(out) <- "survnma"
  # return(out)
  # }

  # check parameter dimensionality
  # dim 1 for exponential
  # dim 2 for FP order 1 (incl Gompertz, Weibull etc)
  # dim 3 for FP order 2
  dimensions <- c("exponential" = 1,
                  "gompertz" = 2,
                  "weibull" = 2,
                  "loglogistic" = 2,
                  "lognormal" = 2,
                  "fp1" = 2,
                  "fp2" = 3)
  dim <- dimensions[[model]]
  # prepare data inputs
  inputs <- prepare_winbugs_data(nma_df, min_time_change)
  # append P values if model is fp1 or fp2
  if(model == "fp1") {
    if(is.null(P) || length(P) != 1)
      stop("Need to supply P (numeric argument) for fractional polynomial model of order one.")
    inputs[["P1"]] <- P[1]

  } else if(model == "fp2") {
    if(is.null(P) || length(P) != 2)
      stop("Need to supply P vector, c(P1,P2), for fractional polynomial model of order two.")
    inputs[["P1"]] <- P[1]
    inputs[["P2"]] <- P[2]
  }

  # prepare prior inputs
  # Note the workaround: we supply dim+1 values, not just dim,
  # to get around case of dim==1 changing the class of our objects
  # (It's stupid but done this way for compatibility with BUGS models.)
  dim_fixed <- dim + (dim==1)
  if(is.null(prior)) {
    inputs[["mean"]] <- rep(0, dim_fixed)
    inputs[["prec2"]] <- 0.001*diag(dim_fixed)
    if(type == "random") #additional prior for RE models
      inputs[["R"]] <- diag(dim_fixed)
  } else {
    # Custom prior values - to be implemented.
    inputs[["mean"]]  <- prior[["mean"]]
    inputs[["prec2"]] <- prior[["prec2"]]
    if(type == "random") #additional prior for RE models
      inputs[["R"]]   <- prior[["R"]]
  }

  # prepare initial values
  # user can define them manually via ... (same as in WinBUGS!)
  if(!is.null(inits) && !is.list(inits))
    if(inits == "generate")
      inits <- prep_winbugs_inits2(type, inputs$NS, inputs$NT,
                                   n.chains, dim)

  # determine values returned by the model
  # we can add 'h' for calculations in the future
  if(type == "random")
    returned_param <- c("mu", "d", "omega") #RE models return extra IW parameter
  else
    returned_param <- c("mu", "d") #FE models don't

  fit <- R2WinBUGS::bugs(data = inputs,
                         inits=inits,
                         model.file=model_path,
                         bugs.directory=bugs.directory,
                         parameters=returned_param,
                         n.chains = n.chains,
                         n.iter = n.iter,
                         ...)

  if(file.exists(model_path))
    file.remove(model_path)

  out <- structure(list(
    fit = fit,
    treatments = unique(nma_df$treatment),
    trt_labels = attr(inputs, "trt_labels"),
    study_labels = attr(inputs, "study_labels"),
    inits = inits,
    inputs = inputs,
    model = model,
    type = type,
    tmax = max(inputs$time),
    nparam = as.numeric(dim),
    P = P,
    data = nma_df))
  class(out) <- "survnma"

  if(auto_restart == TRUE){
    loop <- 1
    new_niter <- n.iter
    while(max(out$fit$summary[,"Rhat"]) > 1.05 & loop < 6){
      new_inits <- extract_init(out, out$fit$n.chains)
      new_niter <- 2*new_niter
      out <- survnma(nma_df,
                     model,
                     type,
                     prior,
                     P,
                     inits = new_inits,
                     n.iter = new_niter,
                     n.chains = n.chains,
                     min_time_change = min_time_change,
                     auto_restart = FALSE,
                     warnings = FALSE)

      loop <- loop + 1
    }
  }

  if((max(out$fit$summary[,"Rhat"]) > 1.05) && auto_restart && warnings){
    message("THE MODEL HAS NOT CONVERGED AFTER 5 LOOPS.")
    message("TAKE INTO CONSIDERATION WHEN MAKING INFERENCES")
  }

  if(max(out$fit$summary[,"Rhat"]) > 1.05 && !auto_restart && warnings){
    message("THE MODEL HAS NOT CONVERGED.")
    message("consider re-running the model with higher n.iter or different priors, initial values")
  }
  out
}
