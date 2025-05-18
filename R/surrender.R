#' Perform surrender action
#'
#' @param hand A character vector of the player's current hand
#' @param allow_surrender Logical. TRUE if game allows surrender at this point
#' @return A list with status, payout, and message
#' @examples
#' surrender_hand(c("10", "6"), TRUE)
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

