context("Testing survnma outputs / inputs")

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

# test_that("data can be generated", {
#   df <-data.frame()
#   for(i in 1:4)
#     df <- rbind(df, data.frame(treatment = c("Suni", "Cabo", "Suni", "Pazo")[i],
#                                study = c("Study 1", "Study 1", "Study 2", "Study 2")[i],
#                                baseline = c("Suni", "Suni", "Suni", "Suni")[i],
#                                generate_km(generate_pop(100, "exponential", 1, 1, 1, 1))))
#   df <-
#     tibble::as_tibble(df) %>%
#     tidyr::nest(-treatment, -study, -baseline, .key = "km")
#
#   expect_is()
# })


test_that("all Ouwens paper models run", {
  sn1 <- survnma(df, "exponential", min_time_change = 0.05)
  sn2 <- survnma(df, "weibull", min_time_change = 0.05)
  sn3 <- survnma(df, "gompertz",  min_time_change = 0.05)
  sn4 <- survnma(df, "lognormal",  min_time_change = 0.05)
  sn5 <- survnma(df, "loglogistic", min_time_change = 0.05)

  expect_is(sn1, "survnma")
  expect_is(sn2, "survnma")
  expect_is(sn3, "survnma")
  expect_is(sn4, "survnma")
  expect_is(sn5, "survnma")
})
