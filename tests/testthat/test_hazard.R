context("Hazard curve/plot testing")

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

nma.weibull <- survnma(df, "weibull", n.iter = 10000, min_time_change = 0.05)
prep.haz <- prep_all_hazards(nma.weibull, "Suni", "Suni")


test_that("Check that ref vs ref gives 1", {
  check.ref <- prep.haz[prep.haz$label == "Suni (reference)", ]
  expect_equal(unique(check.ref$mean), 1)
})

test_that("output", {
  expect_is(prep.haz, "data.frame")

})

