#' Perform a double down action for a player
#'
#' @param hand A character vector of the player's current hand (must be 2 cards)
#' @param deck The current deck
#' @return A list: new_hand (hand after drawing one more card), deck (updated), valid (TRUE/FALSE)
#' @examples
#' result <- double_down(c("5", "6"), deck)
double_down <- function(hand, deck) {
  if (length(hand) != 2) {
    return(list(new_hand = hand, deck = deck, valid = FALSE))
  }

  deal <- deal_cards(deck)
  hand <- c(hand, deal$card)
  deck <- deal$deck

  list(new_hand = hand, deck = deck, valid = TRUE)
}


