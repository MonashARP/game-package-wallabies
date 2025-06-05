library(testthat)

test_that("play_blackjack runs and returns expected structure", {
  skip_if_not(interactive(), "Skipping interactive test in non-interactive environment")

  # Temporarily simulate user input using mockery or run manually
  result <- tryCatch({
    play_blackjack()
  }, error = function(e) e)

  expect_type(result, "list")
  expect_true(all(c("player_hands", "dealer_hand", "player_scores", "dealer_score", "outcomes") %in% names(result)))
  expect_true(length(result$player_scores) >= 1)
})
