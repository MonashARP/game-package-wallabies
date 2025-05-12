#' Deal Cards to Players and Dealer
#'
#' @param num_players Integer. The number of players in the game.
#' @param deck A vector representing the shuffled deck.
#' @return A list with hands for players and dealer.
#' @examples
#' deck <- create_deck()
#' game <- deal_cards(2, deck)
#' @export
deal_cards <- function(num_players, deck, split = FALSE) {
  player_hands <- vector("list", num_players)
  dealer_hand <- deck[1:2]
  deck <- deck[-(1:2)]  # Remove the first two cards (dealer)

  for (i in 1:num_players) {
    if (split) {
      player_hands[[i]] <- list(deck[1:2], deck[3])  # Split into two hands
      deck <- deck[-(1:3)]  # Remove dealt cards
    } else {
      player_hands[[i]] <- deck[1:2]
      deck <- deck[-(1:2)]  # Remove dealt cards
    }
  }

  return(list(player_hands = player_hands, dealer_hand = dealer_hand, deck = deck))
}
