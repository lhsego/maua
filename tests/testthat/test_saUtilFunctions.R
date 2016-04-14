context("saUtilFunctions.R")

test_that("saUtilFunctions() performs as expected", {

  expect_true(all(c("saUtil_exp", "saUtil_log") %in% saUtilFunctions()))

  expect_true(saUtilFunctions("saUtil_exp"))

  expect_true(!saUtilFunctions("noUtil"))

  expect_error(saUtilFunctions(7), "'saUtilFun' must be NULL or")
              
  expect_error(saUtilFunctions(c("t", "y")), "'saUtilFun' must be NULL or")
    
})
