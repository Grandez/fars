###############################################################################
## test.R ---
## Author          : Javier Gonzalez
## Document created: 2016/01/20
## Last modified   : 2016/01/20
###############################################################################
##Purpose:

Sys.setenv("R_TESTS" = "")
library(JGGFars)
library(testthat)


context("Simple tests")

test_that("Test simple", {
  expect_that(sqrt(3) * sqrt(3), equals(3))
})


test_that("Test filename", {
  expect_that( make_filename(2014), equals("accident_2014.csv.bz2"))
})


context("Specific tests")


