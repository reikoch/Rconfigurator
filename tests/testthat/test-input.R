test_that("Rcpp retrievable", {
  expect_true(consistent('3.1.2', 'Rcpp', '1.0.2'))
})

test_that("R version too old", {
  rc <- consistent('2.4.12', 'Rcpp', '1.0.2')
  expect_false(rc)
  expect_named(rc, 'R')
})

test_that("missing package versions listed", {
  expect_error(consistent('3.1.0', c('abc', 'BCE'), c('1.0', '1.0')),
                 'Some package versions could not be found on CRAN:\nBCE 1.0 \n')
})

test_that("missing packages listed", {
  rc <- consistent('3.6.0', 'BCE', '2.1')
  ac <- c(FME=FALSE, limSolve=FALSE, Matrix=FALSE, R=TRUE)
  expect_equal(rc[order(names(rc))], ac[order(names(ac))])  # protect against sort order differences
})
