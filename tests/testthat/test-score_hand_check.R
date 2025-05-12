library(testthat)
test_that("Score calculation works", {
  score <- score_hand(c("A", "9"))
  expect_equal(score, 20)
})
