#' Create a Shuffled Deck of Cards
#'
#'
#' @return A shuffled deck of cards.
#' @examples
#'
#' create_deck()
#' @export

create_deck <- function() {
  deck <- c(2:10, "J", "Q", "K", "A")
  deck <- rep(deck, 4)  # Four suits
  deck <- sample(deck)  # Shuffle the deck
  return(deck)
}
