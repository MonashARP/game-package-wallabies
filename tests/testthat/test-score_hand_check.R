library(testthat)
test_that("score_hand calculates the correct score, adjusting for aces", {
  # Test a hand with Ace (as 11)
  expect_equal(score_hand(c("A♠", "9♣")), 20)  # Ace (11) + 9 = 20

  # Test a hand with multiple Aces
  expect_equal(score_hand(c("A♠", "A♦", "9♠")), 21)  # Ace (11) + Ace (11) + 9 = 21

  # Test a hand with three Aces, should adjust to 13 (Ace counts as 1 and 1)
  expect_equal(score_hand(c("A♠", "A♦", "A♣")), 13)  # Ace (11) + Ace (1) + Ace (1) = 13

  # Test a hand with face cards (Jack, Queen, King, all count as 10)
  expect_equal(score_hand(c("K♠", "Q♣", "10♦")), 30)  # K + Q + 10 = 30

  # Test a hand with Ace (as 1) due to busting
  expect_equal(score_hand(c("A♠", "A♦", "A♣", "10♠")), 13)  # Ace (1) + Ace (1) + Ace (1) + 10 = 13
})

