#' check_connected
#'
#' A simple check to see if the network provided is connected or not.
#'  Meta-Analysis should be carried only on connected networks. It is used directly in survnma as
#'  a stopping mechanism for networks that are not valid.
#'
#' @param df the input dataframe used for carrying the network meta-analysis
#'
#' @return boolean indicating whether network is connected or not. A valid datafame would return TRUE.
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph is.connected
#' @export
#'

check_connected <- function(df){
  edges <- df[df$treatment != df$baseline,c("treatment", "baseline")]
  gr <- igraph::graph_from_data_frame(edges, directed = FALSE)
  out <- igraph::is.connected(gr)
  return(out)
}


#' check_ipd
#'
#'  A simple function that checks if the supplied dataframe is of the necessary format
#'
#' @param df input dataframe
#' @return Boolean TRUE/FALSE
#' @noRd

check_ipd <- function(df){

  if(any(df[,1] == 0)){
    stop("Time in dataframe contains point equal to zero.")
  }
}



#' check_km
#'
#'  A simple function that checks if the supplied dataframe is of the necessary format
#'
#' @param df input dataframe
#' @return Boolean TRUE/FALSE
#' @noRd


check_km <- function(df){
  increasing_t <- all(diff(df[,1]) >= 0)
  if(increasing_t == FALSE){
    stop("Time column is non-increasing")
  }
  if(any(df[,2] < df[,3]) == TRUE){
    stop("More events than possible. N < R at some point")
  }
  if(any(df[,3] < 0)){
    stop("Event indicators must be positive.")
  }
}



#' fixDt
#'
#' function that changes any dataframe (KMdata) in such
#' a way that data points with time difference less than
#' a threshold are removed.
#'
#' This fixes any likelihood / deviance problems
#'
#' @param data dataframe to be modified
#' @param threshold Threshold for minimum change in time accepted
#'                  Logical range of 0.01-0.05
#' @noRd



fixDt <- function(data, threshold = 0.05){
  data$groupTime <- floor(data$time/threshold)
  for(group in unique(data$groupTime)){
    data$newTime[data$groupTime == group] <- min(data$time[data$groupTime == group])
    data$newTotal[data$groupTime == group] <- max(data$n[data$groupTime == group])
    data$newEvent[data$groupTime == group] <- sum(data$r[data$groupTime == group])
    # data$newCensor[data$groupTime == group] <- sum(data$V4[data$groupTime == group])
  }
  newdata <- unique(data[, c("groupTime", "newTime", "newTotal", "newEvent")])
  newdata <- newdata[, 2:4]
  names(newdata) <- c("time", "n", "r")
  return(newdata)
}
