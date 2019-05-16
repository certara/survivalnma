extract_init <- function(survnma, n.chains, omega = TRUE){
  #extracting dimensionalities
  no.studies <- length(unique(survnma$data$study))
  no.treatments <- length(unique(survnma$data$treatment))
  n.param <- survnma$nparam

  # constructing matrices
    # dmean: d mean       dsd: d standard deviation
    # mumean: mu mean     musd: mu standard deviation
  dmean <- matrix(NA, no.treatments, n.param)
  dsd <- matrix(NA, no.treatments, n.param)

  mumean <- matrix(NA, no.studies, n.param)
  musd <- matrix(NA, no.studies, n.param)

  for(i in 2:no.treatments){
    for(j in 1:n.param){
      if(n.param == 1){
        dmean[i] <- mean(survnma$fit$sims.matrix[, paste0("d[", i,"]")])
        dsd[i] <- sd(survnma$fit$sims.matrix[, paste0("d[", i,"]")])
      }else{
        dmean[i,j] <- mean(survnma$fit$sims.matrix[, paste0("d[", i, ",", j, "]")])
        dsd[i,j] <- sd(survnma$fit$sims.matrix[, paste0("d[", i, ",", j, "]")])
      }
    }
  }
  for(i in 1:no.studies){
    for(j in 1:n.param){
      if(n.param == 1){
        mumean[i] <- mean(survnma$fit$sims.matrix[, paste0("mu[", i,"]")])
        musd[i] <- sd(survnma$fit$sims.matrix[, paste0("mu[", i, "]")])
      }else{
        mumean[i,j] <- mean(survnma$fit$sims.matrix[, paste0("mu[", i, ",", j, "]")])
        musd[i,j] <- sd(survnma$fit$sims.matrix[, paste0("mu[", i, ",", j, "]")])
      }
    }
  }
  res <- list()
  for(chain in 1:n.chains){
    d <- matrix(NA, nrow = no.treatments, ncol = n.param)
    if(n.param == 1){
      d[-1] <- matrix(mapply(function(x,y) rnorm(1,x,y), x = dmean[-1], y = dsd[-1]), nrow = (no.treatments - 1))
    }else{
      d[-1,] <- matrix(mapply(function(x,y) rnorm(1,x,y), x = dmean[-1,], y = dsd[-1,]), nrow = (no.treatments - 1))
    }

    mu <- matrix(mapply(function(x,y) rnorm(1,x,y), x = mumean, y=musd), nrow= no.studies)

    res[[chain]] <- list("d" = d,
                     # "omega" = diag(n.param),
                     # "delta" = matrix(0, no.studies, n.param),
                     "mu" = mu)
  }

  if(!omega){
    res <- lapply(res, function(x) {x <- x[c("d", "mu")]; x})
  }

  return(res)
}
