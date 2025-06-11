#' Score the Cards in a Hand
#'
#' Calculates the total score for a Blackjack hand. Aces count as 11 unless it causes a bust.
#'
#' @param hand A character vector of cards.
#' @return An integer representing the total score.
#' @examples
#' score_hand(c("A♠", "10♣"))  # 21
#' score_hand(c("A♠", "A♦", "9♠"))  # 21
#' score_hand(c("A♠", "A♦", "A♣", "10♠"))  # 13
#' @export
score_hand <- function(hand) {
  cpp_score_hand(hand)
}
