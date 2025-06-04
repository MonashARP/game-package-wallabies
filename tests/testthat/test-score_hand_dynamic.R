library(testthat)

test_that("score_hand_dynamic works with standard and custom rules", {

  # Test 1: Standard Rule - Ace is 11 unless the total score is over 21
  result_standard <- score_hand_dynamic(c("A♠", "9♣"))  # Expected: 20 (Ace is 11, 9 is 9)
  expect_equal(result_standard, 20)

  result_standard_multiple_aces <- score_hand_dynamic(c("A♠", "A♦", "9♠"))  # Expected: 21 (Ace is 11, 9 is 9, total = 20)
  expect_equal(result_standard_multiple_aces, 21)

  result_standard_bust <- score_hand_dynamic(c("A♠", "A♦", "A♣", "10♠"))  # Expected: 13 (Adjust the Aces to avoid bust)
  expect_equal(result_standard_bust, 13)

  # Test 2: Custom Rule - Ace is always treated as 1
  result_custom <- score_hand_dynamic(c("A♠", "9♣"), rule = "custom")  # Expected: 10 (Ace is treated as 1, 9 is 9)
  expect_equal(result_custom, 10)

  result_custom_multiple_aces <- score_hand_dynamic(c("A♠", "A♦", "9♠"), rule = "custom")  # Expected: 11 (Ace is treated as 1)
  expect_equal(result_custom_multiple_aces, 11)

  result_custom_bust <- score_hand_dynamic(c("A♠", "A♦", "A♣", "10♠"), rule = "custom")  # Expected: 3 (All Aces treated as 1)
  expect_equal(result_custom_bust, 13)

  # Test 3: Edge cases
  result_no_aces <- score_hand_dynamic(c("10♠", "8♦"), rule = "custom")  # Expected: 18 (No Aces involved)
  expect_equal(result_no_aces, 18)

  result_only_ace <- score_hand_dynamic(c("A♠"), rule = "custom")  # Expected: 1 (Ace treated as 1)
  expect_equal(result_only_ace, 1)

  # Test 4: Invalid rule
  expect_error(score_hand_dynamic(c("A♠", "9♣"), rule = "invalid"), "Unknown rule. Use 'standard' or 'custom'.")
})
