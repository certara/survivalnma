#' add_baseline_column
#'
#' given a data.frame with treatments and studies
#' return the same df but with extra column 'baseline'
#' that automatically selects some baseline for each study
#' you can choose your preferred 'global' baseline manually
#'
#' @param df km dataframe where baseline is to be added
#' @param preferred_baseline specifying a desired baseline
#' @return returns a dataframe
#'
#' @export


add_baseline_column <- function(df, preferred_baseline = NULL) {
  lvls <- unique(df$treatment)
  if(!is.null(preferred_baseline)){
    if(!(preferred_baseline %in% df$treatment))
      stop("Preferred baseline is not in data!")
    n <- which(lvls == preferred_baseline)
    lvls[n] <- lvls[1]
    lvls[1] <- preferred_baseline
  }
  numbering <- as.numeric(factor(df$treatment, levels = lvls))
  df$baseline <- ""
  for(i in 1:(nrow(df)-1)) {
    for(j in (i+1):nrow(df)) {
      if(df$study[j] == df$study[i]) {
        if(numbering[j] > numbering[i]){
          df$baseline[i] <- df$treatment[i]
          df$baseline[j] <- df$treatment[i]
        } else {
          df$baseline[i] <- df$treatment[j]
          df$baseline[j] <- df$treatment[j]
        }
      }
    }
  }
  df
}
