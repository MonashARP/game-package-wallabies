#' Perform a double down action for a player
#'
#' Player adds exactly one card to their initial 2-card hand.
#'
#' @param hand Character vector of player hand
#' @param deck Character vector of remaining deck
#'
#' @return A list with updated hand, updated deck, and validity flag
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
