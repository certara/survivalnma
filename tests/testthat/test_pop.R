context("Testing generating random populations")

test_that("generate_pop returns dataframe", {
  pop1 <- generate_pop(10, "weibull", 0.4, 0.4, 0.4)
  expect_is(pop1, "data.frame")
  expect_equal(dim(pop1)[1], 10)
  expect_equal(dim(pop1)[2], 2)
})

test_that("generate_pop accepts correct families", {
  expect_error(generate_pop(10, "not a family", 0.4, 0.4, 0.4))
})

test_that("generate_pop asks for parameters", {
  expect_error(generate_pop(10, "weibull", p1 = 0.4, p2= 0.4))
  expect_error(generate_pop(10, "weibull", p1 = 0.4, lambda = 1.2))
})

test_that("generate_pop handles censoring correctly", {
  pop2 <- generate_pop(10, "lognormal", 0.4, 0.4, 0.001)
  events <- pop2[,2]

  expect_equal(unique(events), 1)

  expect_error(generate_pop(10, "weibull", p1 = 0.2, p2 = 0.4, lambda = -2))
})
