context("network testing")

disconnected <- data.frame(treatment = c("A","B","C","X","A","A","B","C","X","Y"),
                 baseline = c("A","A","A","Y","B","C","A","A","X","Y"))

empty <- data.frame()


test_that("disconnected networks stop survnma", {
  expect_error(survnma(disconnected,"weibull"))
})

test_that("connectedness testing", {
  expect_equal(check_connected(disconnected), FALSE)
  expect_error(check_connected(empty))
  expect_error(network_plot(empty))
})
