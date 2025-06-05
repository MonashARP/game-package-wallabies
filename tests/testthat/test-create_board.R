library(testthat)

test_that("create_board returns a correctly shuffled deck", {
  # Test 1: Default 4 decks should return 208 cards
  deck <- create_board()
  expect_length(deck, 52 * 4)

  # Test 2: Custom number of decks returns correct length
  deck_6 <- create_board(num_decks = 6)
  expect_length(deck_6, 52 * 6)

  # Test 3: All expected cards are present in a single deck
  suits <- c("♠", "♥", "♦", "♣")
  ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
  single_deck <- paste0(rep(ranks, each = 4), rep(suits, times = 13))
  full_deck_2 <- rep(single_deck, 2)

  expect_true(all(single_deck %in% create_board(num_decks = 1)))

  expect_equal(
    unname(sort(table(create_board(num_decks = 2)))),
    unname(sort(table(full_deck_2)))
  )

  # Test 4: Shuffling gives different orders
  deck1 <- create_board()
  deck2 <- create_board()
  expect_false(identical(deck1, deck2))  # Very unlikely to be identical
})


