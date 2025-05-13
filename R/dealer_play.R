#' Dealer's play logic according to Blackjack rules
#'
#' The dealer continues to draw cards from the deck until their hand reaches at least 17 points,
#' or until they hold 5 cards without busting (known as "five-card Charlie").
#'
#' @param dealer_hand A character vector of the dealer's current hand (e.g., c("10♠", "6♥"))
#' @param deck A character vector representing the current remaining deck
#'
#' @return A list with:
#' \describe{
#' \item{dealer_hand}{The dealer's final hand}
#' \item{deck}{The remaining deck after dealer draws}
#' }
#'
#' @examples
#' deck <- create_board(num_decks = 1)
#' game <- deal_cards(num_players = 2, deck = deck)
#' result <- dealer_play(game$dealer_hand, game$deck)
#' result$dealer_hand
#'
#' @export
dealer_play <- function(dealer_hand, deck) {
  score <- score_hand(dealer_hand)

  while ((score$total < 17 || (score$total == 17 && score$is_soft)) &&
         length(dealer_hand) < 5 && !score$is_bust) {
    dealer_hand <- c(dealer_hand, deck[1])
    deck <- deck[-1]
    score <- score_hand(dealer_hand)
  }

  list(dealer_hand = dealer_hand, deck = deck)
}
