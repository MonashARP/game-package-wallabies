library(testthat)
test_that("Deck creation works", {
  deck <- create_deck()
  expect_length(deck, 52)
  expect_true(all(deck %in% c(2:10, "J", "Q", "K", "A")))
})
