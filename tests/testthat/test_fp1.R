context("Testing FP1 families")

df <- data.frame(
  stringsAsFactors = FALSE,
  "treatment" = c("Suni", "Ifn", "Suni", "Pazo"),
  "study" = c("Study 1", "Study 1", "Study 2", "Study 2"),
  "baseline" = c("Suni", "Suni", "Suni", "Suni"),
  "filepath" = sapply(c("Mota_OS_Suni_KM.txt",
                        "Mota_OS_Ifn_KM.txt",
                        "Mot_OS_Suni_KM.txt",
                        "Mot_OS_Pazo_KM.txt"), function(x)
                          system.file("extdata", "narrow", x, package="survnma", mustWork=TRUE)))


fp1.families <- list( list(model = "fp1", P = c(0), name = "fp1 model with P = 0"),
                      list(model = "fp1", P = c(1), name = "fp1 model with P = 1"),
                      list(model = "fp1", P = c(0.5), name = "fp1 model with P = 0.5"),
                      list(model = "fp1", P = c(-1), name = "fp1 model with P = -1"),
                      list(model = "fp1", P = c(-0.5), name = "fp1 model with P = -0.5")
)

for(model in fp1.families){

  nma.fixed <- survnma(df, model = model$model, P= model$P, type = "fixed", min_time_change = 0.05)
  nma.random <- survnma(df, model = model$model, P= model$P, type = "random", min_time_change = 0.05)

  test_that("class of nmas", {
    expect_is(nma.fixed, "survnma")
    expect_is(nma.random, "survnma")
  })

  test_that("dimensionality", {

    cols.f <- ((length(nma.fixed$treatments) - 1) * nma.fixed$nparam * 2) + 1
    cols.r <- ((length(nma.random$treatments)) * nma.random$nparam * 2) + 1
    matrix.cols.f <- dim(nma.fixed$fit$sims.matrix)[2]
    matrix.cols.r <- dim(nma.random$fit$sims.matrix)[2]

    expect_equal(matrix.cols.f, cols.f)
    expect_equal(matrix.cols.r, cols.r)
  })

  test_that(paste0("convergence message for fixed effect ", model$name), {
    expect_message(survnma(df, model = model$model, P= model$P, n.iter = 10, type = "fixed", min_time_change = 0.05))
  })

  test_that(paste0("convergence message for random effect ", model$name), {
    expect_message(survnma(df, model = model$model, P= model$P, n.iter = 10, type = "random", min_time_change = 0.05))
  })

  test_that("Requests powers", {
    expect_error(survnma(df, model = fp1.families[[3]]$model, P = c(-1,-1,-1), min_time_change = 0.05))
    expect_error(survnma(df, model = fp1.families[[3]]$model, P = NULL, min_time_change = 0.05))

    expect_error(survnma(df, model = fp1.families[[3]]$model, P = c(-1,-1,-1), type = "random", min_time_change = 0.05))
    expect_error(survnma(df, model = fp1.families[[3]]$model, P = NULL, type = "random", min_time_change = 0.05))
  })
}
