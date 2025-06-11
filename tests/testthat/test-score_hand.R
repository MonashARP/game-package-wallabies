library(testthat)

test_that("cpp_score_hand handles Aces correctly", {
  expect_equal(cpp_score_hand(c("A♠", "10♣")), 21)
  expect_equal(cpp_score_hand(c("A♠", "A♦", "9♠")), 21)
  expect_equal(cpp_score_hand(c("A♠", "A♦", "A♣", "10♠")), 13)
})

test_that("cpp_score_hand throws error for invalid card", {
  expect_error(cpp_score_hand(c("Z♠", "9♠")))
})


