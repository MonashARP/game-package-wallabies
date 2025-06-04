library(testthat)

test_that("cpp_score_hand works for normal hands", {
  expect_equal(cpp_score_hand(c("A♠", "9♦")), 20)
  expect_equal(cpp_score_hand(c("10♣", "K♦")), 20)
  expect_equal(cpp_score_hand(c("3♣", "4♦", "5♥")), 12)
})

test_that("cpp_score_hand handles multiple Aces correctly", {
  expect_equal(cpp_score_hand(c("A♠", "A♦", "9♠")), 21)
  expect_equal(cpp_score_hand(c("A♠", "A♦", "A♣", "9♠")), 12)
})

test_that("cpp_score_hand busts when over 21", {
  expect_equal(cpp_score_hand(c("K♠", "Q♦", "5♣")), 25)
})

test_that("cpp_score_hand errors on invalid card", {
  expect_error(cpp_score_hand(c("Z♠", "10♠")), "Invalid card rank")
})

