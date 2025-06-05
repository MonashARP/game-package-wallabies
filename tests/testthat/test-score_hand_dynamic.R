library(testthat)

test_that("cpp_score_hand_dynamic works for standard hands", {
  expect_equal(cpp_score_hand_dynamic(c("A♠", "9♦")), 20)
  expect_equal(cpp_score_hand_dynamic(c("10♣", "Q♦")), 20)
  expect_equal(cpp_score_hand_dynamic(c("5♣", "6♦", "9♠")), 20)
})

test_that("cpp_score_hand_dynamic correctly handles multiple Aces (standard)", {
  expect_equal(cpp_score_hand_dynamic(c("A♠", "A♦", "8♠")), 20)  # One Ace is 11, other is 1
  expect_equal(cpp_score_hand_dynamic(c("A♠", "A♦", "A♣", "8♠")), 21)  # Two Aces as 1, one as 11
})

test_that("cpp_score_hand_dynamic works with custom rule (Ace always 1)", {
  expect_equal(cpp_score_hand_dynamic(c("A♠", "9♦"), rule = "custom"), 10)
  expect_equal(cpp_score_hand_dynamic(c("A♠", "A♦", "9♠"), rule = "custom"), 11)
})

test_that("cpp_score_hand_dynamic busts over 21", {
  expect_equal(cpp_score_hand_dynamic(c("K♠", "Q♦", "5♣")), 25)
})

test_that("cpp_score_hand_dynamic errors on invalid card rank", {
  expect_error(cpp_score_hand_dynamic(c("Z♠", "10♠")), "Invalid card rank")
})

test_that("cpp_score_hand_dynamic errors on invalid rule", {
  expect_error(cpp_score_hand_dynamic(c("A♠", "10♠"), rule = "weird"), "Unknown rule")
})
