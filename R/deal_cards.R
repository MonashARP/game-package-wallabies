#' Deal Cards to Players and Dealer
#'
#' @param num_players Integer. The number of players in the game.
#' @param deck A vector representing the shuffled deck.
#' @return A list with hands for players and dealer.
#' @examples
#' deck <- create_deck()
#' game <- deal_cards(2, deck)
#' @export
deal_cards <- function(num_players, deck) {
  player_hands <- list()
  dealer_hand <- sample(deck, 2)
  deck <- setdiff(deck, dealer_hand)

  for (i in 1:num_players) {
    player_hands[[i]] <- sample(deck, 2)
    deck <- setdiff(deck, player_hands[[i]])
  }

  return(list(player_hands = player_hands, dealer_hand = dealer_hand, deck = deck))
}
