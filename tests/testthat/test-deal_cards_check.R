library(testthat)
test_that("deal_cards deals two cards to the dealer and each player", {
  deck <- create_board(num_decks = 1)

  # Test dealing with 2 players
  game <- deal_cards(2, deck)

  # Ensure that two cards are dealt to the dealer
  expect_length(game$dealer_hand, 2)

  # Ensure that two cards are dealt to each player
  expect_length(game$player_hands[[1]], 2)
  expect_length(game$player_hands[[2]], 2)

  # Ensure deck size decreases after dealing
  expect_length(game$deck, 52 - (2 * 2 + 2))  # Two cards per player and dealer (2 players)

  # Test splitting
  deck_split <- create_board(num_decks = 1)
  game_split <- deal_cards(1, deck_split, split = TRUE)  # Test with split

  # Ensure split results in two hands for the player (only if the first two cards are the same)
  rank_1 <- substr(game_split$player_hands[[1]][[1]], 1, 1)
  rank_2 <- substr(game_split$player_hands[[1]][[2]], 1, 1)

  if (rank_1 == rank_2) {
    expect_length(game_split$player_hands[[1]], 1)  # First hand (should have 1 card)
    expect_length(game_split$player_hands[[2]], 1)  # Second hand (should have 1 card)
  } else {
    expect_length(game_split$player_hands[[1]], 2)  # No split, first hand (should have 2 cards)
    expect_length(game_split$player_hands, 1)  # Only one hand in case of no split
  }
})
