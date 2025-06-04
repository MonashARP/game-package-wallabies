library(testthat)

test_that("dealer_play draws until 17 or 5 cards (with dynamic scoring)", {
  # Create a custom deck so we can control draws
  preset_deck <- c("2♠", "3♦", "4♣", "5♥", "6♠", "7♦", "8♣", "9♥", "10♠", "J♦")

  # Case 1: Dealer starts with low total (score below 17)
  result <- dealer_play(dealer_hand = c("2♣", "3♠"), deck = preset_deck, rule = "standard")
  final_score <- score_hand_dynamic(result$dealer_hand, rule = "standard")

  # Dealer should draw until the score is at least 17, or 5 cards have been dealt
  expect_true(final_score >= 17 || length(result$dealer_hand) == 5)
  expect_true(length(result$dealer_hand) >= 2)  # Dealer should have at least 2 cards

  # Case 2: Dealer already has 17 or more — should not draw
  result2 <- dealer_play(dealer_hand = c("10♣", "7♠"), deck = preset_deck, rule = "standard")
  expect_equal(result2$dealer_hand, c("10♣", "7♠"))
  expect_equal(result2$deck, preset_deck)

  # Case 3: Soft 17 forces dealer to draw
  result3 <- dealer_play(dealer_hand = c("A♠", "6♦"), deck = preset_deck, rule = "standard")
  final_score3 <- score_hand_dynamic(result3$dealer_hand, rule = "standard")
  expect_true(length(result3$dealer_hand) > 2)  # Dealer should have drawn more cards
  expect_true(final_score3 > 17)  # Soft 17 rule applies, dealer must draw more cards

  # Case 4: Dealer reaches 5 cards without busting
  result4 <- dealer_play(dealer_hand = c("2♠", "3♠"), deck = c("2♦", "3♦", "4♣", "5♥", "6♠"), rule = "standard")
  expect_equal(length(result4$dealer_hand), 5)  # Dealer should have exactly 5 cards
  final_score4 <- score_hand_dynamic(result4$dealer_hand, rule = "standard")
  expect_true(final_score4 <= 21)  # Ensure dealer did not bust
})
