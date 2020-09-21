context("Survival curve testing")

# stored for testing
res1 <- survival_curve("weibull", matrix(c(1, 1), 1, 2), c(1, 2, 3, 4, 5))
res2 <- survival_curve("exponential",matrix(c(1)), c(1,2,3,4,5),type="mean")
res3 <- survival_curve("gompertz", matrix(c(1,2),2,2), c(1,2,3,4,5), type= "interval")
res4 <- survival_curve("loglogistic", matrix(c(1,0),4,4), c(1,2,3), type= "mean")

check1 <- survival_curve("weibull", matrix(c(1,1), 10, 2), c(0,Inf), type = "mean" )
check2 <- survival_curve("exponential", matrix(c(2,3), 10 , 1), c(0,Inf), type = "mean")
check3 <- survival_curve("lognormal", matrix(c(0,log(1)), 10, 2, byrow= TRUE), c(0, exp(1.96), Inf), type= "mean")
check4 <- survival_curve("lognormal", matrix(c(0,log(1)), 10, 2, byrow= TRUE), c(0, exp(-1.96), Inf), type= "mean")
check5 <- survival_curve("fp1", matrix(c(2,3), 10, 2), c(0,2,3,4,5), P=c(4), type = "all")

test_that("survival_curve returns correct object", {

  expect_is(res1, "matrix")
  expect_is(res4, "numeric")
  expect_is(survival_curve("lognormal", matrix(c(0.2,0.2),10,10), c(5,2)), "matrix")
  expect_equal(dim(check5)[1], 10)
  expect_equal(dim(check5)[2], 5)
})


  # warning/error tests
test_that("survival_curve detects warnings/errors", {
  expect_warning(survival_curve("weibull", matrix(c(2,3),10,10), 10), "Length of time vector should be at least 2")
  expect_warning(survival_curve("lognormal", matrix(c(0.2,0.2),10,10), c(5)), "Length of time vector should be at least 2")
  expect_error(survival_curve("fp2", matrix(c(1, 1, 2, 2), 2, 2), c(1, 2, 3, 4, 5), P = -1), "Power vector needs to be of length 2")
  expect_error(survival_curve("fp1", matrix(c(1, 1, 2, 2), 2, 2), c(1, 2, 3, 4, 5), P = NULL), "Power vector cannot be NULL")
  expect_error(survival_curve("Wrong Family", matrix(c(1, 2, 3, 4), 2, 2), c(1,2,3,4), P= 1), "Wrong or unspecified family of distributions")
})

  # numeric tests
test_that("survival_curve outputs correct numeric values", {
  expect_equal(check1[1], 1)
  expect_equal(check1[2], 0)
  expect_equal(check2[1], 1)
  expect_equal(check2[2], 0)
  expect_equal(check3[2], 0.025, tolerance = 0.001)
  expect_equal(check4[2], 0.975, tolerance = 0.001)
  })



