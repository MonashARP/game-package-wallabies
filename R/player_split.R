#' Player Split Function
#'
#' Checks if a player's hand can be split and returns split hands if allowed.
#'
#' @param hand A character vector of two cards (e.g., c("A♠", "A♥"))
#' @return A list with can_split (logical) and hands (list of hands)
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
