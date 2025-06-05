#' Player Split Function
#'
#' Checks if a player's hand can be split and returns split hands if allowed.
#'
#' @param hand A character vector of two cards (e.g., c("A♠", "A♥"))
#'
#' @return A list with:
#' \describe{
#'   \item{can_split}{Logical indicating if a split is allowed.}
#'   \item{hands}{A list containing either the original hand or two split hands.}
#' }
#'
#' @examples
#' # Example 1: Splitting a hand with a pair of Aces
#' player_split(c("A♠", "A♥"))
#'
#' # Example 2: Cannot split non-matching cards
#' player_split(c("A♠", "10♠"))
#'
#' # Example 3: Invalid hand with more than 2 cards (split not allowed)
#' player_split(c("10♠", "10♥", "5♣"))
#'
#' @export
player_split <- function(hand) {
  if (length(hand) != 2) {
    return(list(can_split = FALSE, hands = list(hand)))
  }

  rank_1 <- strsplit(hand[1], "[♠♦♥♣]")[[1]][1]
  rank_2 <- strsplit(hand[2], "[♠♦♥♣]")[[1]][1]

  if (rank_1 == rank_2) {
    return(list(
      can_split = TRUE,
      hands = list(c(hand[1]), c(hand[2]))
    ))
  } else {
    return(list(
      can_split = FALSE,
      hands = list(hand)
    ))
  }
}
