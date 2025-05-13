library(testthat)

test_that("dealer stops at hard 17", {
  dealer_hand <- c("10♠", "7♦") # 17 total
  deck <- c("2♣", "3♣", "4♣")
  result <- dealer_play(dealer_hand, deck)

  expect_equal(score_hand(result$dealer_hand), 17)
  expect_equal(length(result$dealer_hand), 2)
})

test_that("dealer hits below 17", {
  dealer_hand <- c("6♠", "8♦") # 14
  deck <- c("3♣", "2♣", "5♦")
  result <- dealer_play(dealer_hand, deck)
  final_score <- score_hand(result$dealer_hand)

  expect_true(final_score >= 17 || length(result$dealer_hand) == 5)
})

test_that("dealer stops at 5 cards", {
  dealer_hand <- c("2♠", "2♦") # 4
  deck <- c("3♣", "4♣", "5♦", "2♣", "3♦")
  result <- dealer_play(dealer_hand, deck)

  expect_equal(length(result$dealer_hand), 5)
  expect_lte(score_hand(result$dealer_hand), 21)
})

test_that("dealer can bust", {
  dealer_hand <- c("10♠", "6♣") # 16
  deck <- c("10♦") # → 26
  result <- dealer_play(dealer_hand, deck)

  expect_gt(score_hand(result$dealer_hand), 21)
})
