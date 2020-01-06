# internal only
# given extraction data.frame (from .csv)
# return a well-formated matrix that represents this one study&trt
# combination in the input matrix
convert_from_digitizer <- function(input, t, b, s) {
  
  #this '1000' value is a fake placeholder value
  #note how it's omitted in the output, so it doesn't matter
  input$time <- c(input[-1,"time"], 1000)
  input$dt <- input$time - c(0, input[-nrow(input),"time"])
  #input$n <- input$V2          # number of patients at risk in the interval
  #input$r <- input$V3          # number of events in the interval
  input$t <- t                 # treatment
  input$b <- b                 # baseline treatment
  input$s <- s                 # study
  return(input[-nrow(input),c("time", "dt", "n", "r", "t", "b", "s")])
}

# given a data.frame with columns 'study', 'baseline' and 'treatment'
# together with character 'filepath' or list of df's km' (see below)
# prepare complete list of inputs into r2winbugs model
prepare_winbugs_data <- function(df, min_time_change = 0) {
  ll <- list()

  # Study labelling (ugly but nucessary):
  df$study <- factor(df$study)
  # store the labels:
  study_labels <- 1:length(levels(df$study))
  names(study_labels) <- levels(df$study)
  df$study <- as.numeric(df$study)

  # Treatment labelling (derp derp derp):
  # make sure levels in treatment and baseline cols agree
  # and follow the same numbering scheme!
  aval_trts <- unique(as.character(df$treatment))
  bsl_trts <- unique(as.character(df$baseline))
  lvls <- c(bsl_trts, aval_trts[!(aval_trts %in% bsl_trts)])
  df$treatment <- as.numeric(factor(df$treatment, levels = lvls))
  df$baseline <- as.numeric(factor(df$baseline, levels = lvls))
  # store the labels:
  trt_labels <- 1:length(lvls)
  names(trt_labels) <- lvls

  if('filepath' %in% names(df)){
    if('km' %in% names(df))
      message(
        paste("Both file paths and KM data specified in input data.frame().",
              "Using 'km' column, not 'filepath'."))
  } else if (!('km' %in% names(df))) {
    stop(paste0("Either 'filepath' or 'km' column need to be specified in input data.
         See ?survnma."))
  }
  for(i in 1:nrow(df)){
    if('filepath' %in% names(df)){
      km <- utils::read.table(df$filepath[i], header=T)
    }else{
      km <- df$km[[i]]
    }
    
    # Make sure column names are time, n, r, c
    km <- as.data.frame(as.matrix(km))
    km <- km[, 1:3]
    names(km) <- c("time", "n", "r")
    
    
    bad_inputs <- c()
    if(min_time_change == 0){
      if(min(diff(km$time)) < 0.01){
        bad_inputs <- c(bad_inputs, i)
      }
    }else{
      km <- fixDt(km)
    }
    ll[[i]] <- convert_from_digitizer(km,
                                      t = df$treatment[i],
                                      b = df$baseline[i],
                                      s = df$study[i])
  }
  if(length(bad_inputs) > 0)
    warning(paste("Some time periods are zero.",
               "Consider setting min_time_change = 0.05. See ?survnma"))

  winbugsi <- do.call(rbind, ll)
  if(any(winbugsi[,3] <= 0) ){
    warning(paste("Input values contained ", sum(winbugsi[,3] == 0),
                  " zeroes. These values have been removed"))
    winbugsi <- subset(winbugsi, (winbugsi[,3] > 0))
  }
  if(any(winbugsi[,4] > winbugsi[,3])){
    warning("There was R > N. Removing such data")
    winbugsi <- subset(winbugsi, !(winbugsi[,4] > winbugsi[,3]))
  }
  # This solution will only work for 2-arm studies for now:
  # ts <- df$treatment
  # bs <- df$baseline
  df_bsls <- df[df$treatment == df$baseline,]
  df_trts <- df[df$treatment != df$baseline,]
  ts <- df_trts$treatment[order(df_trts$study)]
  bs <- df_bsls$treatment[order(df_bsls$study)]
  ns <- max(df$study)
  nt <- max(df$treatment)

  # Workaround for analysing only 1 study
  # Without it R->WinBUGS converts to double (rather than vector)
  if(length(ts) == 1){
    ts <- c(ts, 0)
    bs <- c(bs, 0)
  }

  bugs_input <- list(
    "N"=nrow(winbugsi), "NS"=ns, "NT"=nt,
    # mean, prec2 are prior values
    # in case of RE you also need a prior on R
    # all 3 are defined separately within survnma()
    "time"=winbugsi[,1], "dt"=winbugsi[,2], "n"=winbugsi[,3], "r"=winbugsi[,4],
    "t"=winbugsi[,5], "b"=winbugsi[,6], "s"=winbugsi[,7],
    "ts"=ts, "bs"=bs)

  structure(bugs_input,
            trt_labels = trt_labels,
            study_labels = study_labels)
}
