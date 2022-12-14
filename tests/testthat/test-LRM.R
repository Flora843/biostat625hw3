
test_that("LRM works", {
  y=1:2
  x=1:2
  expect_equal(LRM(y~x), -1)
  y=1:100
  x=1:100
  z=1:100
  expect_equal(LRM(y~x+z), -1)
  expect_length(LRM(mpg~wt+drat,data=mtcars), 5)
})

test_that("LRM works", {
  coefs.actual <- as.vector(LRM(mpg~wt+drat,data=mtcars)$LRM_coefs[,1])
  coefs.expected <- as.vector(summary(lm(mpg~wt+drat,data=mtcars))$coefficients[,1])
  expect_equal(coefs.actual, coefs.expected)
})

test_that("LRM works", {
  coefs.actual <- as.vector(LRM(mpg~wt+drat,data=mtcars)$LRM_coefs[,3])
  coefs.expected <- as.vector(summary(lm(mpg~wt+drat,data=mtcars))$coefficients[,3])
  expect_equal(coefs.actual, coefs.expected)
})

test_that("LRM works", {
  coefs.actual <- as.vector(LRM(mpg~wt+drat,data=mtcars)$F.statistics$value)
  coefs.expected <- as.vector(summary(lm(mpg~wt+drat,data=mtcars))$fstatistic[1])
  expect_equal(coefs.actual, coefs.expected)
})
test_that("LRM works", {
  coefs.actual <- as.vector(LRM(mpg~wt+drat,data=mtcars)$R.squared$R_squared)
  coefs.expected <- as.vector(summary(lm(mpg~wt+drat,data=mtcars))$r.squared)
  expect_equal(coefs.actual, coefs.expected)
})
test_that("LRM works", {
  coefs.actual <- as.vector(LRM(mpg~wt+drat,data=mtcars)$R.squared$Adjusted.R.squared)
  coefs.expected <- as.vector(summary(lm(mpg~wt+drat,data=mtcars))$adj.r.squared)
  expect_equal(coefs.actual, coefs.expected)
})
test_that("LRM works", {
  coefs.actual<-as.vector(LRM(mpg~wt+drat,data=mtcars)$Confidence.Interval[1,1])
  coefs.expected<-as.vector(confint(lm(mpg~wt+drat,data=mtcars))[1,1])
  expect_equal(coefs.actual, coefs.expected)
  coefs.actual<-as.vector(LRM(mpg~wt+drat,data=mtcars)$Confidence.Interval[1,2])
  coefs.expected<-as.vector(confint(lm(mpg~wt+drat,data=mtcars))[1,2])
  expect_equal(coefs.actual, coefs.expected)
})


