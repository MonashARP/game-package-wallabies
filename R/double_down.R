#' Perform a double down action for a player
#'
#' Player adds exactly one card to their initial 2-card hand.
#'
#' @param hand Character vector of player hand
#' @param deck Character vector of remaining deck
#'
#' @return A list with:
#' \describe{
#'   \item{new_hand}{The player's hand after drawing one additional card.}
#'   \item{deck}{The updated deck after removing one card.}
#'   \item{valid}{Logical indicating whether the double down was valid (only valid for 2-card hands).}
#' }
#'
#' @examples
#' # Example setup
#' hand <- c("5♠", "6♦")
#' deck <- c("9♣", "10♦", "3♥")  # top of deck is 9♣
#'
#' # Perform double down
#' result <- double_down(hand, deck)
#' result$new_hand   # Expected: c("5♠", "6♦", "9♣")
#' result$deck       # Expected: c("10♦", "3♥")
#' result$valid      # Expected: TRUE
#'
#' # Invalid example (hand is not 2 cards)
#' double_down(c("5♠", "6♦", "2♥"), deck)
#'
#' @export
double_down <- function(hand, deck) {
  if (length(hand) != 2) {
    return(list(new_hand = hand, deck = deck, valid = FALSE))
  }

  new_card <- deck[1]
  new_hand <- c(hand, new_card)
  deck <- deck[-1]

  list(new_hand = new_hand, deck = deck, valid = TRUE)
}
