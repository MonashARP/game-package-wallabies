library(testthat)

test_that("dealer_play stops at hard 17 or above", {
  deck <- c("2♣", "3♦", "4♥", "5♠", "6♣", "7♦", "8♥")
  dealer_hand <- c("10♠", "7♣")  # score = 17

  result <- dealer_play(dealer_hand, deck)
  expect_equal(result$dealer_hand, dealer_hand)  # no new card drawn
})

test_that("dealer_play hits below 17", {
  deck <- c("5♣", "3♦", "2♠")
  dealer_hand <- c("9♣", "6♦")  # score = 15

  result <- dealer_play(dealer_hand, deck)
  expect_gt(score_hand(result$dealer_hand), 15)
  expect_lt(length(result$dealer_hand), 6)
})

test_that("dealer_play hits on soft 17", {
  deck <- c("3♣", "4♦", "5♠")
  dealer_hand <- c("A♠", "6♠")  # soft 17

  result <- dealer_play(dealer_hand, deck)
  expect_gt(score_hand(result$dealer_hand), 17)
})

test_that("dealer_play stops at 5 cards", {
  deck <- c("2♠", "3♠", "2♦", "2♣", "2♥")
  dealer_hand <- c("2♦", "2♣")  # starts at 4

  result <- dealer_play(dealer_hand, deck)
  expect_lte(length(result$dealer_hand), 5)
})

test_that("dealer_play returns remaining deck correctly", {
  deck <- c("2♠", "3♠", "4♠", "5♠", "6♠")
  dealer_hand <- c("A♠", "5♦")  # score = 16

  result <- dealer_play(dealer_hand, deck)
  total_used <- length(result$dealer_hand) - 2
  expect_equal(length(result$deck), length(deck) - total_used)
})
