#' Create a Shuffled Deck of Cards
#'
#' @param num_decks The number of decks shuffled in the game
#' @return A shuffled deck of cards.
#' @examples
#' # Create a single shuffled deck
#' deck1 <- create_board(1)
#' head(deck1)
#'
#' # Create and view number of cards in 6 decks
#' deck6 <- create_board(6)
#' length(deck6)  # Should be 312
#'
#' @export

create_board <- function(num_decks = 4) {
  suits <- c("♠", "♥", "♦", "♣")
  ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
  single_deck <- paste0(rep(ranks, each = 4), rep(suits, times = 13))
  full_deck <- rep(single_deck, num_decks)
  sample(full_deck)
}
