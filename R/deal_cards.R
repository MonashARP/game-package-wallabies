#' Deal Cards to Players and Dealer
#'
#' This function deals cards to the specified number of players and a dealer.
#' Each player receives two cards, and the dealer receives two cards.
#' Optionally, hands can be split.
#'
#' @param num_players Integer. Number of players in the game.
#' @param deck A vector representing the shuffled deck of cards.
#' @param split Logical. If TRUE, the function allows for splitting hands.
#' @return A list containing the player hands, dealer hand, and remaining deck.
#' @examples
#' # Create a shuffled deck with 1 deck (52 cards)
#' deck <- create_board(num_decks = 1)
#'
#' # Deal cards to 2 players
#' game <- deal_cards(num_players = 2, deck)
#'
#' # Display the hands of the players and the dealer
#' game$player_hands  # Player hands
#' game$dealer_hand   # Dealer hand
#'
#' # Create a shuffled deck with 4 decks (208 cards)
#' deck_full <- create_board(num_decks = 4)
#'
#' # Deal cards to 2 players with a full deck
#' game_full <- deal_cards(num_players = 2, deck_full)
#'
#' # Display the hands of the players and the dealer using a full deck
#' game_full$player_hands  # Player hands
#' game_full$dealer_hand   # Dealer hand
#'
#' @export

deal_cards <- function(num_players, deck, split = FALSE) {
  player_hands <- vector("list", num_players)
  dealer_hand <- deck[1:2]
  deck <- deck[-(1:2)]  # Remove dealer's cards

  for (i in 1:num_players) {
    if (split) {
      # Extract the rank part of the card strings (e.g., "A♥" -> "A")
      rank_1 <- substr(deck[1], 1, 1)  # Extract rank of the first card (e.g., "A" from "A♥")
      rank_2 <- substr(deck[2], 1, 1)  # Extract rank of the second card (e.g., "A" from "A♠")

      if (rank_1 == rank_2) {  # Only split if the first two cards are the same rank
        player_hands[[i]] <- list(deck[1], deck[2])  # Split into two separate hands
        deck <- deck[-(1:2)]  # Remove dealt cards
      } else {
        player_hands[[i]] <- list(deck[1], deck[2])  # No split, just two cards
        deck <- deck[-(1:2)]  # Remove dealt cards
      }
    } else {
      player_hands[[i]] <- list(deck[1], deck[2])  # No split, just two cards
      deck <- deck[-(1:2)]  # Remove dealt cards
    }
  }

  return(list(player_hands = player_hands, dealer_hand = dealer_hand, deck = deck))
}
