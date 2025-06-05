#' Perform surrender action
#'
#' @param hand A character vector of the player's current hand
#' @param allow_surrender Logical. TRUE if game allows surrender at this point
#' @return A list with status, payout, and message
#' @examples
#' # Example 1: Player surrenders with two cards and surrender is allowed
#' surrender_hand(c("10♠", "6♦"), TRUE)
#'
#' # Example 2: Surrender is not allowed
#' surrender_hand(c("10♠", "6♦"), FALSE)
#'
#' # Example 3: Hand has more than two cards (surrender invalid)
#' surrender_hand(c("10♠", "6♦", "2♣"), TRUE)
#' @export
surrender_hand <- function(hand, allow_surrender = TRUE) {
  if (allow_surrender && length(hand) == 2) {
    return(list(
      status = "surrendered",
      payout = 0.5,
      message = "Player surrendered. Half bet lost."
    ))
  } else {
    return(list(
      status = "played",
      payout = 0,
      message = "Surrender not allowed or invalid hand."
    ))
  }
}

