# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(LRM)

test_that("LRM works", {
  coefs.actual <- as.vector(LRM(mpg~wt+drat,data=mtcars)$LRM.coefs[,1])
  coefs.expected <- as.vector(summary(lm(mpg~wt+drat,data=mtcars))$coefficients[,1])
  expect_equal(coefs.actual, coefs.expected)
})

test_that("LRM works", {
  coefs.actual <- as.vector(LRM(mpg~wt+drat,data=mtcars)$LRM.coefs[,3])
  coefs.expected <- as.vector(summary(lm(mpg~wt+drat,data=mtcars))$coefficients[,3])
  expect_equal(coefs.actual, coefs.expected)
})

test_that("LRM works", {
  coefs.actual <- as.vector(LRM(mpg~wt+drat,data=mtcars)$F.statistics$value)
  coefs.expected <- as.vector(summary(lm(mpg~wt+drat,data=mtcars))$fstatistic[1])
  expect_equal(coefs.actual, coefs.expected)
})
