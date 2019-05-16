#'
#' hazard_plot
#'
#'  A function to be used with data* passed through prep_all_hazards. It plots the hazard-ratio curves
#'  for indicated treatments. Alternatively, one can use a survnma object directly. The function will
#'  then calculate the hazards internally. Execution is identical in either case.
#'
#' @param data a dataframe obtained directly through function \code{\link{prep_all_hazards}}
#'             or a survnma object
#' @param treatments a list of treatments you want to plot
#' @param reference string representing reference treatment
#' @param timesteps Vector of evaluation times. Should be of length > 2
#' @param study_labs a collection of labels for the studies
#' @param colors a collection of colors for graphing
#' @param xlab a string to be used for labelling the x-axis
#' @param ylab a string to be used for labelling the y-axis
#' @param xlim specifying a limit for the x-axis
#' @param ylim specifying a limit for the y-axis
#' @param title title to be used for the hazard plot
#' @param interval boolean whether you want a credible interval on the curve
#'
#' @return a figure of required hazard-ratio plots
#'
#' @export

hazard_plot <- function(data= NULL,
                        treatments = NULL,
                        reference = NULL,
                        timesteps = seq(0,30),
                        study_labs = NA,
                        colors=NULL,
                        xlab="",
                        ylab="",
                        xlim = NULL,
                        ylim = NULL,
                        title="",
                        interval = FALSE) {

  if(!(class(data) %in% c("data.frame", "survnma")))
    stop("You must provide a data.frame or a survnma object to be used")
  else if(class(data) == "survnma") {
    if(is.null(treatments))
      treatments <- data$treatments

    if(is.null(reference)){
      message(paste("No `reference` argument provided. Using", treatments[1], "as reference"))
      reference <- treatments[1]
    }
    data <- prep_all_hazards(data, treatments, reference, timesteps)
  }

  uci <- lci <- label <- NULL

  ggplot(data, aes(x=t)) +
    geom_line(aes(y=mean, group=label, colour=label), size=1.5) +
    geom_abline(intercept= 1, slope =0)+
    # scale_colour_manual(values=colors) +
    {if(interval) geom_ribbon(aes(ymax=uci, ymin=lci, group=label, fill=label), alpha=.1)} +
    # geom_ribbon(aes(ymax=u75, ymin=l25, group=curve, fill=curve), alpha=.2) +
    # scale_fill_manual(values=colors)  +
    xlab(xlab) + ylab(ylab) +
    {if(is.null(xlab)) xlab("Time")} +
    coord_cartesian(ylim = c(0, 2)) +
    ggtitle(title) +
    {if(!is.null(xlim)) coord_cartesian(xlim=xlim)} +
    {if(!is.null(ylim)) coord_cartesian(ylim=ylim)} +
    # scale_x_continuous(breaks=c(0,6,12,18,24,30,36)) +
    theme(legend.title = element_blank(), legend.position = c(1,1), legend.justification = c(1,1))
    # {if(!is.null(xlim)) coord_cartesian(x = xlim, default = TRUE)}
}

