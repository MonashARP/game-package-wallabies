library(testthat)

test_that("double_down draws one card for valid 2-card hand", {
  hand <- c("5♠", "6♦")
  deck <- c("10♥", "3♣", "Q♦")
  result <- double_down(hand, deck)

  expect_equal(length(result$new_hand), 3)
  expect_equal(result$new_hand[3], "10♥")
  expect_equal(result$deck, c("3♣", "Q♦"))
  expect_true(result$valid)
})

test_that("double_down fails with non-2-card hand", {
  hand <- c("5♠", "6♦", "2♣")
  deck <- c("10♥", "Q♦")
  result <- double_down(hand, deck)

  expect_equal(result$new_hand, hand)
  expect_equal(result$deck, deck)
  expect_false(result$valid)
})

test_that("double_down removes only one card from deck", {
  hand <- c("8♠", "2♦")
  deck <- c("A♣", "5♦", "K♠")
  result <- double_down(hand, deck)

  expect_equal(result$new_hand[3], "A♣")
  expect_equal(result$deck, c("5♦", "K♠"))
})

