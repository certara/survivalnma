# to be used for summary

survnma_vcov <- function(fit, study) {
  mu <- extract_mu(fit, study)
  df <- data.frame()
  for(trt in fit$treatments) {
    d <- extract_d(fit, trt)
    df <- rbind(df, data.frame(
      treatment = trt,
      mean = apply(mu + d, 2, mean),
      var(mu +d)))
  }
  rownames(df) <- NULL
  colnames(df) <- c("treatment", "mean", paste0("var", 1:fit$nparam))
  df
}

