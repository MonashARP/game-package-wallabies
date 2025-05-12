#' Score the Cards in a Hand
#'
#' @param hand A vector of cards.
#' @return The score of the hand.
#' @examples
#'
#' score_hand(c("A", "9"))
#' @export
score_hand <- function(hand) {
  values <- c(2:10, 10, 10, 10, 11)
  card_values <- values[match(hand, c(2:10, "J", "Q", "K", "A"))]
  score <- sum(card_values)

  # Adjust for Ace (if necessary)
  aces <- sum(card_values == 11)
  while (score > 21 && aces > 0) {
    score <- score - 10
    aces <- aces - 1
  }

  score
}
