#' Create a Shuffled Deck of Cards
#'
#' @param num_decks The number of decks shuffled in the game
#' @return A shuffled deck of cards.
#' @examples
#'
#' create_board()
#' @export

create_board <- function(num_decks = 4) {
  suits <- c("♠", "♥", "♦", "♣")
  ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
  single_deck <- paste0(rep(ranks, each = 4), rep(suits, times = 13))
  full_deck <- rep(single_deck, num_decks)
  sample(full_deck)
}
