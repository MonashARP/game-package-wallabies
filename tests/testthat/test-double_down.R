library(testthat)


test_that("double_down correctly updates hand and deck", {

  # Create a custom deck with known values for controlled testing
  preset_deck <- c("3♠", "7♦", "J♣", "4♠", "A♥")

  # Case 1: Valid double down (2 cards in hand)
  hand <- c("5♠", "6♦")  # Player's hand with 2 cards
  result <- double_down(hand, preset_deck)  # Perform double down

  # Check if hand is updated correctly
  expect_equal(result$new_hand, c("5♠", "6♦", "3♠"))  # Hand should now have 3 cards
  expect_equal(result$deck, c("7♦", "J♣", "4♠", "A♥"))  # One card should be drawn from the deck
  expect_true(result$valid)  # It should be valid since the hand had 2 cards

  # Case 2: Invalid double down (more than 2 cards in hand)
  hand_invalid <- c("5♠", "6♦", "3♥")  # Player's hand with 3 cards
  result_invalid <- double_down(hand_invalid, preset_deck)

  # Check that hand and deck remain unchanged
  expect_equal(result_invalid$new_hand, hand_invalid)  # Hand should not change
  expect_equal(result_invalid$deck, preset_deck)  # Deck should not change
  expect_false(result_invalid$valid)  # It should be invalid since the hand had more than 2 cards

  # Case 3: Invalid double down (empty hand)
  hand_empty <- c()  # Empty hand
  result_empty <- double_down(hand_empty, preset_deck)

  # Check that hand and deck remain unchanged
  expect_equal(result_empty$new_hand, hand_empty)  # Hand should not change
  expect_equal(result_empty$deck, preset_deck)  # Deck should not change
  expect_false(result_empty$valid)  # It should be invalid since the hand had 0 cards

})
