#' Create a Shuffled Deck of Cards
#'
#'
#' @return A shuffled deck of cards.
#' @examples
#'
#' create_deck()
#' @export

create_deck <- function() {
  deck <- rep(c(2:10, "J", "Q", "K", "A"), 4)  # Four suits
  sample(deck)  # Return shuffled deck
}
