library(testthat)

test_that("double_down adds one card to 2-card hand", {
  hand <- c("5", "6")
  deck <- c("10", "J", "Q")

  result <- double_down(hand, deck)

  expect_equal(length(result$new_hand), 3)
  expect_equal(result$new_hand[3], "10")
  expect_equal(result$deck, c("J", "Q"))
  expect_true(result$valid)
})

test_that("double_down does not work with hand of more than 2 cards", {
  hand <- c("5", "6", "2")
  deck <- c("10", "J", "Q")

  result <- double_down(hand, deck)

  expect_equal(result$new_hand, hand)
  expect_equal(result$deck, deck)
  expect_false(result$valid)
})

test_that("double_down removes correct card from deck", {
  hand <- c("8", "2")
  deck <- c("K", "3", "4")

  result <- double_down(hand, deck)

  expect_equal(result$new_hand[3], "K")
  expect_equal(result$deck, c("3", "4"))
})
