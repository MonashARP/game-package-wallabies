library(testthat)
test_that("Card dealing works", {
  deck <- create_deck()
  game <- deal_cards(2, deck)
  expect_length(game$player_hands, 2)
  expect_length(game$dealer_hand, 2)
})
