#' Score the Cards in a Hand
#'
#' @param hand A vector of cards.
#' @return The score of the hand.
#' @examples
#' # Score a hand with Ace and 9
#' score_hand(c("A", "9"))
#'
#' # Score a hand with multiple Aces
#' score_hand(c("A", "A", "9"))
#'
#' # Score a hand with multiple Aces and a 10, resulting in a bust
#' score_hand(c("A", "A", "A", "10"))

#' @export
score_hand <- function(hand) {
  values <- c(2:10, 10, 10, 10, 11)  # Values for 2-10, Jack, Queen, King, Ace
  card_values <- values[match(hand, c(2:10, "J", "Q", "K", "A"))]
  score <- sum(card_values)

  # Adjust for Aces: If score is over 21 and there are Aces, reduce Ace value from 11 to 1
  aces <- sum(card_values == 11)
  while (score > 21 && aces > 0) {
    score <- score - 10
    aces <- aces - 1
  }

  score
}
