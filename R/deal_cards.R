#' Deal Cards to Players and Dealer
#'
#' Deals 2 cards to each player and 1 card to the dealer. Dealer's 2nd card comes later.
#'
#' @param num_players Number of players
#' @param deck A shuffled deck of cards
#' @return A list with player_hands, dealer_hand (1 card), and remaining deck
#' @examples
#' # Example: Deal to 2 players from a fresh deck
#' deck <- create_board(1)
#' result <- deal_cards(num_players = 2, deck)
#' result$player_hands  # list of two hands
#' result$dealer_hand   # single card
#' length(result$deck)  # 52 - 2*2 - 1 = 47
#' @export
deal_cards <- function(num_players, deck) {
  player_hands <- vector("list", num_players)

  # Each player gets 2 cards
  for (i in 1:num_players) {
    player_hands[[i]] <- deck[1:2]
    deck <- deck[-(1:2)]
  }

  # Dealer gets 1 card
  dealer_hand <- deck[1]
  deck <- deck[-1]

  return(list(
    player_hands = player_hands,
    dealer_hand = dealer_hand,
    deck = deck
  ))
}
