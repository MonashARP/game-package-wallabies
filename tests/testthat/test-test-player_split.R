library(testthat)
test_that("player_split works correctly", {
  # Test for a valid split
  hand <- c("A♠", "A♣")
  result <- player_split(hand)
  expect_true(result$can_split)
  expect_length(result$hands, 2)

  # Test for invalid split (hand with different ranks)
  hand <- c("A♠", "K♣")
  result <- player_split(hand)
  expect_false(result$can_split)
  expect_length(result$hands, 1)
})
