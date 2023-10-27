library(testthat)
library(MASS)

#Test the first coefficient
test_that("ridgereg() get similar coefficients as lm.ridge()",{
  ridgereg_coef <- ridgereg:::ridgereg$new(Petal.Length~Species, iris, 0.001)
  lm_ridge_coef <- MASS::lm.ridge(Petal.Length~Species, iris)
  a <- ridgereg_coef$coef()
  b <- coef(lm_ridge_coef)
  expect_equal(round(a[1],3), round(as.numeric(sub("^\\[1\\] ", "", b[1])),3),
               info = "The first coefficients returned should be the same")
})
#Test the second coefficient
test_that("ridgereg() get similar coefficients as lm.ridge()",{
  ridgereg_coef <- ridgereg:::ridgereg$new(Petal.Length~Species, iris, 0.001)
  lm_ridge_coef <- MASS::lm.ridge(Petal.Length~Species, iris)
  a <- ridgereg_coef$coef()
  b <- coef(lm_ridge_coef)
  expect_equal(round(a[2],3), round(as.numeric(sub("^\\[1\\] ", "", b[2])),3),
               info = "The second coefficients returned should be the same")
})
#Test the third coefficient
test_that("ridgereg() get similar coefficients as lm.ridge()",{
  ridgereg_coef <- ridgereg:::ridgereg$new(Petal.Length~Species, iris, 0.001)
  lm_ridge_coef <- MASS::lm.ridge(Petal.Length~Species, iris)
  a <- ridgereg_coef$coef()
  b <- coef(lm_ridge_coef)
  expect_equal(round(a[3],3), round(as.numeric(sub("^\\[1\\] ", "", b[3])),3),
               info = "The third coefficients returned should be the same")
})


