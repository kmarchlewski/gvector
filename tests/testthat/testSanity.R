library(gvector)
context("Simple sanity tests")

test_that("Numeric element", {
  w = V(1)
  expect_that(w, is_a("gvector"))
  expect_equal(w[[1]],1)
})

test_that("Numeric elements", {
  w = V(3,4,5)
  expect_that(w, is_a("gvector"))
  expect_equal(w[[1]],3)
  expect_equal(w[[2]],4)
  expect_equal(w[[3]],5)
})

