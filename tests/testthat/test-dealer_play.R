library(testthat)

test_that("dealer stops at hard 17", {
  dealer_hand <- c("10♠", "7♦")
  deck <- c("2♣", "3♣", "4♣")
  result <- dealer_play(dealer_hand, deck)
  expect_equal(result$dealer_hand, dealer_hand)
})

test_that("dealer hits below 17 and stops at 17", {
  dealer_hand <- c("9♠", "6♦")
  deck <- c("2♣", "3♦", "5♠")
  result <- dealer_play(dealer_hand, deck)
  expect_equal(score_hand(result$dealer_hand)$total, 17)
})

test_that("dealer hits on soft 17", {
  dealer_hand <- c("A♠", "6♦")
  deck <- c("2♣", "3♦", "4♠")
  result <- dealer_play(dealer_hand, deck)
  expect_true(score_hand(result$dealer_hand)$total >= 17)
  expect_true(length(result$dealer_hand) >= 3)
})

test_that("dealer stops at 5 cards if not bust", {
  dealer_hand <- c("2♠", "2♦")
  deck <- c("3♣", "4♣", "5♦", "3♦", "6♠")
  result <- dealer_play(dealer_hand, deck)
  expect_equal(length(result$dealer_hand), 5)
  expect_false(score_hand(result$dealer_hand)$is_bust)
})

test_that("dealer can bust", {
  dealer_hand <- c("10♠", "6♣")
  deck <- c("10♦")
  result <- dealer_play(dealer_hand, deck)
  expect_true(score_hand(result$dealer_hand)$is_bust)
})
