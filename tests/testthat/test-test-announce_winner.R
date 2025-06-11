library(testthat)
test_that("announce_winner works correctly", {
  # Test where player wins
  player_scores <- c(19, 21)
  dealer_score <- 18
  result <- announce_winner(player_scores, dealer_score)
  expect_equal(result, c("Win", "Win"))

  # Test where player ties with dealer
  player_scores <- c(18, 21)
  dealer_score <- 18
  result <- announce_winner(player_scores, dealer_score)
  expect_equal(result, c("Tie", "Win"))

  # Test where player loses
  player_scores <- c(22, 20)
  dealer_score <- 21
  result <- announce_winner(player_scores, dealer_score)
  expect_equal(result, c("Lose", "Lose"))
})
