library(testthat)
test_that("insurance_bet works correctly", {
  # Test where insurance is not available
  dealer_hand <- c("9♠", "10♦")
  result <- insurance_bet(dealer_hand, player_accepts = TRUE)
  expect_false(result$insurance_available)
  expect_equal(result$payout, 0)

  # Test where insurance is available, and player accepts
  dealer_hand <- c("A♠", "10♦")
  result <- insurance_bet(dealer_hand, player_accepts = TRUE)
  expect_true(result$insurance_available)

  # Test where insurance is available, and player declines
  result <- insurance_bet(dealer_hand, player_accepts = FALSE)
  expect_true(result$insurance_available)
  expect_false(result$insurance_paid)
})

