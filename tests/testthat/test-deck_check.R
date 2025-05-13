library(testthat)
test_that("create_board generates the correct number of cards and valid cards", {

  # Test with a single deck (52 cards)
  deck_single <- create_board(num_decks = 1)
  expect_length(deck_single, 52)  # Single deck should have 52 cards

  # Test with multiple decks (e.g., 4 decks should give 208 cards)
  deck_multiple <- create_board(num_decks = 4)
  expect_length(deck_multiple, 52 * 4)  # Four decks should have 208 cards

  # Ensure the deck contains valid cards (check if all cards contain valid ranks and suits)
  valid_ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
  valid_suits <- c("♠", "♥", "♦", "♣")
  valid_cards <- paste0(rep(valid_ranks, each = 4), rep(valid_suits, times = 13))

  expect_true(all(deck_single %in% valid_cards))  # All cards in the single deck should be valid
  expect_true(all(deck_multiple %in% valid_cards))  # All cards in the multiple decks should be valid

  # Check if the deck is shuffled (should not be in a fixed order)
  expect_false(identical(deck_single, sort(deck_single)))  # Deck should not be sorted
  expect_false(identical(deck_multiple, sort(deck_multiple)))  # Deck should not be sorted
})

