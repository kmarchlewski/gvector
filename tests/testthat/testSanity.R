library(gvector)
context("Simple sanity tests")

test_that("Numeric elements", {
  w = V(1)
  expect_equal(w[[1]],1)
})

