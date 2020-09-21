context("Hazard and Survival Table testing")

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

nma.weibull <- survnma(df, "weibull", min_time_change = 0.05)

test_that("Check that monthly and threemonthly give same results", {
  month <- hazard_table(nma.weibull, reference = "Suni", timesteps = seq(0,55,1))
  three <- hazard_table(nma.weibull, reference = "Suni", timesteps = seq(0,55,3))

  months <- c(0.001, seq(1,55,1))
  trimonths <- c(0.001, seq(3,54,3))

  expect_equal(three$time, trimonths)
  expect_equal(month$time, months)

  expect_equal(three$Ifn, month$Ifn[seq(1,55,3)])
})

test_that("Check that monthly and threemonthly give same results", {
  month <- survival_table(nma.weibull, study = "Study 1", timesteps = seq(0,55,1))
  three <- survival_table(nma.weibull, study = "Study 1", timesteps = seq(0,55,3))

  months <- c(0.001, seq(1,55,1))
  trimonths <- c(0.001, seq(3,54,3))

  expect_equal(three$time, trimonths)
  expect_equal(month$time, months)


  expect_equal(three$Ifn, month$Ifn[seq(1,55,3)])
})


