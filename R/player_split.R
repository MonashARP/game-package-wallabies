#' Player Split Function
#'
#' This function checks if a player's hand can be split and splits it if possible.
#'
#' @param hand A vector of two cards representing the player's hand.
#' @return A list containing the split hands or the original hand if splitting is not possible.
#' @examples
#' player_split(c("A♠", "A♥")) # Returns two hands with each Ace.
#' player_split(c("A♠", "10♥")) # Returns the original hand.
#' @export

player_split <- function(hand) {
  if (length(hand) != 2) {
    stop("The hand must contain exactly two cards to check for splitting.")
  }

  # Extract the ranks of the two cards
  rank_1 <- strsplit(hand[1], "[♠♦♥♣]")[[1]][1]
  rank_2 <- strsplit(hand[2], "[♠♦♥♣]")[[1]][1]

  if (rank_1 == rank_2) {
    return(list(hand_1 = hand[1], hand_2 = hand[2]))
  } else {
    return(list(original_hand = hand))
  }
}
