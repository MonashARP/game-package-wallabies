library(testthat)

test_that("deal_cards distributes cards correctly", {
  # Setup: create a full shuffled deck
  deck <- create_board(num_decks = 1)  # 52 cards
  num_players <- 3

  # Run the function
  result <- deal_cards(num_players, deck)

  # Test 1: Correct number of player hands
  expect_length(result$player_hands, num_players)

  # Test 2: Each player gets 2 cards
  for (i in 1:num_players) {
    expect_length(result$player_hands[[i]], 2)
  }

  # Test 3: Dealer gets 1 card
  expect_length(result$dealer_hand, 1)

  # Test 4: Remaining deck has 52 - (2*num_players + 1) cards
  expected_remaining <- 52 - (2 * num_players + 1)
  expect_length(result$deck, expected_remaining)

  # Test 5 (Optional): No card is duplicated
  dealt_cards <- unlist(c(result$player_hands, list(result$dealer_hand)))
  expect_equal(length(unique(dealt_cards)), length(dealt_cards))
})
