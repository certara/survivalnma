context("Testing extraction of parameters")

df <- data.frame(
  stringsAsFactors = FALSE,
  "treatment" = c("Suni", "Ifn", "Suni", "Pazo"),
  "study" = c("Study 1", "Study 1", "Study 2", "Study 2"),
  "baseline" = c("Suni", "Suni", "Suni", "Suni"),
  "filepath" = sapply(c("Mota_OS_Suni_KM.txt",
                        "Mota_OS_Ifn_KM.txt",
                        "Mot_OS_Suni_KM.txt",
                        "Mot_OS_Pazo_KM.txt"), function(x)
                          system.file("extdata", "narrow", x, package="survnma", mustWork=TRUE))
)

nma <- survnma(df, "weibull", min_time_change = 0.05)

test_that("Rubbish study/ treatment causes an error", {
  expect_error(extract_mu(nma, "wrong study"))
  expect_error(extract_d(nma, "wrong treatment"))
})

test_that("Checking format", {
  expect_is(extract_mu(nma, "Study 1"), "matrix")
  expect_is(extract_d(nma, "Ifn"), "matrix")
})

test_that("Testing if baseline", {
  global.base <- names(which(nma$trt_labels == 1))
  expect_equal(unique(c(extract_d(nma, global.base))), 0)
  for(trt in nma$treatments[-1]){
    expect_gt(length(unique(c(extract_d(nma, trt)))), 1)
  }
  expect_equal(unique(c(relative_d_in_study(nma, "Suni", "Study 2"))), 0)
  expect_gt(length(unique(c(relative_d_in_study(nma, "Pazo", "Study 2")))), 1)
})

