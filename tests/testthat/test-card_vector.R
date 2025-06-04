library(testthat)

# Test 1: Creating a card_vector
test_that("card_vector creation works", {
  # Create a card vector
  deck <- card_vector(c("A♠", "10♣", "K♥"))

  # Check the class of the object
  expect_s3_class(deck, "card_vector")

  # Check the content of the card vector by accessing the underlying data
  expect_equal(vctrs::vec_data(deck), c("A♠", "10♣", "K♥"))
})

# Test 2: Extracting card ranks
test_that("card_rank extraction works", {
  deck <- card_vector(c("A♠", "10♣", "K♥"))

  # Check if ranks are correctly extracted
  expect_equal(card_rank(deck), c("A", "10", "K"))
})

# Test 3: Extracting card suits
test_that("card_suit extraction works", {
  deck <- card_vector(c("A♠", "10♣", "K♥"))

  # Check if suits are correctly extracted
  expect_equal(card_suit(deck), c("♠", "♣", "♥"))
})

# Test 4: Checking if cards are face cards
test_that("card_is_face works", {
  deck <- card_vector(c("A♠", "10♣", "K♥"))

  # Check if face cards are correctly identified
  expect_equal(card_is_face(deck), c(FALSE, FALSE, TRUE))
})

# Test 5: Invalid input handling (non-card_vector input)
test_that("Invalid input gives error", {
  # Non-card_vector input
  expect_error(card_rank("A♠"), "inherits(x, \"card_vector\")", fixed = TRUE)
  expect_error(card_suit("A♠"), "inherits(x, \"card_vector\")", fixed = TRUE)
  expect_error(card_is_face("A♠"), "inherits(x, \"card_vector\")", fixed = TRUE)
})

# Test 6: Testing format method for card_vector
test_that("format method for card_vector works", {
  deck <- card_vector(c("A♠", "10♣", "K♥"))

  # Check if cards are formatted correctly
  expect_equal(format(deck), c("[A♠]", "[10♣]", "[K♥]"))
})

