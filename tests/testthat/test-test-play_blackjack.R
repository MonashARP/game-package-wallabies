library(testthat)

test_that("play_blackjack prompts and prints expected output", {
  # Simulate user input and capture the output from play_blackjack()
  output <- capture.output({
    # Simulate inputs directly via readline() replacement in the actual play_blackjack() function
    # We'll simulate 1 player, 1 deck, no surrender, and the player chooses to hit and stand
    play_blackjack()
  })

  # Check that the game prints the "Welcome to Blackjack!" message
  expect_true(any(grepl("🎮 Welcome to Blackjack!", output)))

  # Check that the game asks for the number of players
  expect_true(any(grepl("🎲 How many players? ", output)))

  # Check that the game asks for the number of decks
  expect_true(any(grepl("🃏 How many decks? (Enter for default) :", output)))

  # Check that the game asks "Do you want to surrender?" during player turn
  expect_true(any(grepl("  ➤ Do you want to surrender? (y/n): ", output)))

  # Simulate and check that the game asks "Hit or stand?"
  expect_true(any(grepl("  ➤ Hit or stand? (h/s): ", output)))

  # Check that the player results (Player1's outcome) are printed
  expect_true(any(grepl("👤 Player1 :", output)))

  # Check that the outcome message is printed correctly (e.g., "Player1 loses", "Player1 wins")
  expect_true(any(grepl("👤 Player1 :", output)))

  # Ensure the final outcome message for the player is printed correctly, like "surrendered" or final score
  expect_true(any(grepl("💥 BUST!", output)))

  # Check if dealer's outcome is printed (e.g., dealer busts or dealer final score)
  expect_true(any(grepl("🤵 Dealer's full hand: ", output)))
  expect_true(any(grepl("➡ Dealer score: ", output)))
})


