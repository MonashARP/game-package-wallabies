library(testthat)
test_that("surrender_hand works correctly", {
  # Test where surrender is allowed
  hand <- c("10♠", "6♣")
  result <- surrender_hand(hand, allow_surrender = TRUE)
  expect_equal(result$status, "surrendered")
  expect_equal(result$payout, 0.5)

  # Test where surrender is not allowed
  result <- surrender_hand(hand, allow_surrender = FALSE)
  expect_equal(result$status, "played")
  expect_equal(result$payout, 0)
})

