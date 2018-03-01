context("test-converters.R")

test_that("icc2sem works", {
  expect_equal(icc2sem(icc = 0.36, sd = 1), 0.8)
})

test_that("icc2sem errors", {
  expect_error(icc2sem(icc = 1.2, sd = 1))
})

test_that("sem2icc works", {
  expect_equal(sem2icc(sem = 0.8, sd = 1), 0.36)
})

test_that("extrapRel2sd works", {
  expect_equal(extrapRel2sd(icc_extrap = 0.75, icc_original = 0.5), sqrt(2))
})

test_that("extrapRel2sd errors", {
  expect_error(extrapRel2sd(icc_extrap = 1.75, icc_original = 0.5))
})

test_that("sd2extrapRel works", {
  expect_equal(sd2extrapRel(sd = 2, icc_original = 0.5), 0.875)
})

test_that("sd2extrapRel errors", {
  expect_error(sd2extrapRel(sd = 2, icc_original = 1.5))
})

test_that("d2overlap works", {
  expect_equal(d2overlap(0), 1)
})

test_that("d2u3 works", {
  expect_equal(d2u3(0), 0.5)
})

test_that("d2cles works", {
  expect_equal(d2cles(0), 0.5)
})

test_that("overlap2d works", {
  expect_equal(overlap2d(1), 0)
})

test_that("u32d works", {
  expect_equal(u32d(0.5), 0)
})

test_that("es_convert_d u3 works", {
  expect_equal(es_convert(u3 = 0.5)$d, 0)
})

test_that("es_convert_d d works", {
  expect_equal(es_convert(d = 0)$cles, 0.5)
})
