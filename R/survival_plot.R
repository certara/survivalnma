#' survival_plot
#'
#' Survival plotting function for survnma objects
#'
#' @param data One of the two:
#' \itemize{
#'   \item{a data-frame object (e.g. from \code{\link{prep_all_survivals}} function) containing columns
#'     of \code{c("curve","time", "mean", "lci", "uci", "label")}}
#'   \item{\code{survnma} object; this will lead to "behind-the-scenes"
#'     survival curve calculations;
#'     in that case you should/can also define \code{study},
#'     \code{treatments}, \code{timesteps} (see \code{\link{prep_all_survivals}})}}
#' @param study a specified study to be used for carrying the analysis
#' @param treatments a selection of treatments to be compared; can be the whole set of available treatments
#' @param timesteps Vector of evaluation times. Should be of length > 2
#' @param study_labs labels for each study
#' @param colors vector containing color codes to be used for curves
#' @param xlab labels for x-axis
#' @param ylab labels for y-axis
#' @param xlim a limit for the x-axis
#' @param ylim a limit for the y-axis
#' @param title label for title
#' @param interval boolean for producing credible interval or not
#' @return returns a graph of survival curves
#'
#' @import ggplot2
#'
#' @export



survival_plot <- function(data = NULL,
                          study = NULL,
                          treatments = NULL,
                          timesteps = seq(0,30),
                          study_labs = NA,
                          colors = NULL,
                          xlab = NULL,
                          ylab = "",
                          xlim = NULL,
                          ylim = NULL,
                          title = "",
                          interval = TRUE) {

  if(!(class(data) %in% c("data.frame", "survnma")))
    stop("You must provide either a data.frame or a survnma object to be used")
  else if(class(data) == "survnma") {
    if(is.null(treatments))
      treatments <- data$treatments
    if(is.null(study)){
      message(paste0("'study' argument empty; automatically using ",
                     names(data$study_labels)[1],
                     " as the reference study"))
      study <- names(data$study_labels)[1]
    }
    data <- prep_all_survivals(data, study, treatments, timesteps)
  }

  # Fix for CMD check
  lci <- uci <- label <- NULL

  ggplot(data, aes(x = t)) +
    geom_line(aes(
      y = mean,
      group = label,
      colour = label
    ), size = 1.5) +
    # scale_colour_manual(values=colors) +
    {if (interval) geom_ribbon(aes(ymax = uci, ymin = lci, group = label,
                                   fill = label), alpha = .1) } +
    xlab(xlab) + ylab(ylab) +
    {if(!is.null(xlim)) xlim(xlim)} +
    {if(!is.null(ylim)) ylim(ylim)} +
    {if(is.null(xlab))  xlab("Time")} +
    ggtitle(title) +
    # scale_x_continuous(breaks=c(0,6,12,18,24,30,36)) +
    # coord_cartesian(y = c(0, 1)) +
    # theme_light()
    theme(
      legend.title = element_blank(),
      legend.position = c(1, 1),
      legend.justification = c(1, 1)
    )
}
