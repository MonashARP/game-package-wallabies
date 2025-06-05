#' Score the Cards in a Hand with Dynamic Rules (C++ version)
#'
#' This function calculates the score of a hand using C++ for better performance.
#' It supports the standard Blackjack rules and a custom rule where Aces are always worth 1.
#'
#' @param hand A character vector of cards, e.g., c("A♠", "10♦", "5♣")
#' @param rule A character string indicating the scoring rule to use:
#'   - `"standard"` (default): Aces are worth 11, but reduced to 1 if over 21.
#'   - `"custom"`: Aces are always worth 1.
#'
#' @return An integer score for the hand.
#' @export
#' @examples
#' score_hand_dynamic(c("A♠", "9♣"))            # Standard: 20
#' score_hand_dynamic(c("A♠", "A♦", "9♠"))       # Standard: 21
#' score_hand_dynamic(c("A♠", "A♦", "9♠"), "custom")  # Custom: 11
score_hand_dynamic <- function(hand, rule = "standard") {
  cpp_score_hand_dynamic(hand, rule)
}
