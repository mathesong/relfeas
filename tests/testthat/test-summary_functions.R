context("test-summary_functions.R")

test_that("sumStat_total works with d", {
  expect_equal(sumStat_total(n1 = 20, mean1 = 5, sd1 = 1, d = 0.8)$mean2, 5.8)
})

test_that("sumStat_total works with mean2", {
  expect_equal(sumStat_total(n1 = 20, mean1 = 5, sd1 = 1, mean2 = 5.8)$d, 0.8)
})

test_that("sd_pooled works", {
  expect_equal(sd_pooled(n1 = 10, sd1 = 5, n2 = 15, sd2 = 5), 5)
})

test_that("mean_tot works", {
  expect_equal(mean_tot(n1 = 15, mean1 = 5, n2 = 10, mean2 = 3), 4.2)
})

test_that("sd_tot works", {
  expect_equal(round(sd_tot(n1 = 20, mean1 = 1, sd1 = 1, mean2 = 5), 2), 2.25)
})

test_that("calcD works", {
  expect_equal(calcD(n1 = 10, mean1 = 10, sd1 = 1, mean2 = 11), 1)
})

test_that("sdtot2mean2 works", {
  expect_equal(round(sdtot2mean2(sd_total = 1.2, n1 = 20)$mean2pos, 2), 1.35)
})
