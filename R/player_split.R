#' Player Split Function
#'
#' This function checks if a player's hand can be split and returns individual hands if so.
#'
#' @param hand A character vector of two cards (e.g., c("A♠", "A♥")).
#' @return A list with either two hands or the original hand.
#' @export
player_split <- function(hand) {
  if (length(hand) != 2) {
    stop("Split is only allowed for a 2-card hand.")
  }

  rank_1 <- strsplit(hand[1], "[♠♦♥♣]")[[1]][1]
  rank_2 <- strsplit(hand[2], "[♠♦♥♣]")[[1]][1]

  if (rank_1 == rank_2) {
    return(list(hand_1 = hand[1], hand_2 = hand[2]))
  } else {
    return(list(original_hand = hand))
  }
}
