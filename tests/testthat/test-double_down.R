library(testthat)
library(Blackjack)  # Replace with your actual package name

test_that("double_down behaves correctly with valid and invalid hands", {
  deck <- c("9♠", "7♦", "5♣")

  # Case 1: Valid double down (2-card hand)
  hand <- c("10♠", "6♥")
  result <- double_down(hand, deck)

  expect_length(result$new_hand, 3)
  expect_equal(result$new_hand[1:2], hand)
  expect_equal(result$new_hand[3], deck[1])
  expect_equal(result$deck, deck[-1])
  expect_true(result$valid)

  # Case 2: Invalid double down (hand has 3 cards already)
  hand_invalid <- c("10♠", "6♥", "2♣")
  result_invalid <- double_down(hand_invalid, deck)

  expect_equal(result_invalid$new_hand, hand_invalid)
  expect_equal(result_invalid$deck, deck)
  expect_false(result_invalid$valid)

  # Case 3: Invalid double down (hand has 1 card only)
  hand_invalid2 <- c("A♠")
  result_invalid2 <- double_down(hand_invalid2, deck)

  expect_equal(result_invalid2$new_hand, hand_invalid2)
  expect_equal(result_invalid2$deck, deck)
  expect_false(result_invalid2$valid)
})
