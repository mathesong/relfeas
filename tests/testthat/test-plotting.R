context("test-plotting.R")

test_that("plot_difference d works", {
  expect_type(plot_difference(d = 2), "list")
})

test_that("plot_difference u3 works", {
  expect_type(plot_difference(u3 = 0.8), "list")
})

test_that("plot_difference overlap works", {
  expect_type(plot_difference(overlap = 0.8), "list")
})

test_that("plot_difference cles works", {
  expect_type(plot_difference(cles = 0.8), "list")
})
