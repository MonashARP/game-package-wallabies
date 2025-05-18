library(testthat)

test_that("score_hand calculates correct values", {
  # Case 1: Simple hand with no Ace
  expect_equal(score_hand(c("10♠", "7♦")), 17)

  # Case 2: One Ace counted as 11
  expect_equal(score_hand(c("A♠", "6♣")), 17)

  # Case 3: One Ace adjusted to 1
  expect_equal(score_hand(c("A♠", "9♣", "8♦")), 18)  # A(11) + 9 + 8 -> bust, so A = 1

  # Case 4: Multiple Aces with adjustments
  expect_equal(score_hand(c("A♠", "A♦", "9♠")), 21)  # A(11) + A(1) + 9 = 21

  # Case 5: Multiple Aces adjusted further
  expect_equal(score_hand(c("A♠", "A♦", "A♣", "10♠")), 13)  # A(1) + A(1) + A(1) + 10 = 13

  # Case 6: Bust without Aces
  expect_equal(score_hand(c("10♠", "9♦", "5♣")), 24)

  # Case 7: Invalid card rank triggers error
  expect_error(score_hand(c("Z♠", "10♣")), "Invalid card rank detected")
})


