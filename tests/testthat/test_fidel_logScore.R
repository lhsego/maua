context("fidel_logScore()")

test_that("fidel_logScore() returns values as expected", {

  p1 <- runif(9)
  p2 <- runif(9, 0, 1 - p1)
  p3 <- 1 - p1 - p2
   
  # Notice how the names of the probability columns "prob_7", "prob_9", and "prob_2" have
  # "2", "7", and "9" in them, which are the values of truth column, "verdad"
  X <- data.frame(verdad = factor(rep(c(2, 7, 9), 3)),
                  prob_7 = p1,
                  prob_9 = p2,
                  prob_2 = p3,
                  extra = rnorm(9))

  x1 <- fidel_logScore(X, "verdad", c(FALSE, TRUE, TRUE, TRUE, FALSE), outCol = "ls")$ls

  correctResult <- log(c(p3[1], p1[2], p2[3], p3[4], p1[5], p2[6], p3[7], p1[8], p2[9]))

  expect_equal(x1, correctResult)

}) 

test_that("fidel_logScore() scales correctly", {

  X2 <- data.frame(pa = c(0.25, 1, 0, 0),
                   pb = c(0.25, 0, 0, 0.5),
                   pc = c(0.25, 0, 1, 0.5),
                   pd = c(0.25, 0, 0, 0),
                   tr = factor(c("d", "a", "b", "d"), levels = letters[1:4]))

  x2 <- fidel_logScore(X2, "tr", paste("p", letters[1:4], sep = ""), outCol = "h", scale = TRUE)$h

  expect_equal(x2, c(0, 1, -Inf, -Inf))
    
})

test_that("fidel_logScore() returns errors as expected", {

  expect_error(fidel_logScore(TRUE, "this", "that"), "'X' must be a dataframe")
  expect_error(fidel_logScore(TRUE, "this", "that"), "Error : fidel_logScore(", fixed = TRUE)

  expect_error(fidel_logScore(data.frame(x = 10, y = 12), "p", "q"), "'p' is not in", fixed = TRUE)
  expect_error(fidel_logScore(data.frame(x = 10, y = 12), "x", "q"), "'q' is not in", fixed = TRUE)

  # Continue here
  expect_error(fidel_logScore(data.frame(x = 10, y = 12), c("x", "y"), "x"), "'q' is not in", fixed = TRUE)  
  
    
})
