#' Announce Winner Function
#'
#' This function announces the winner(s) of the game based on hand scores.
#'
#' @param player_scores A named vector of scores for each player.
#' Announce Winner Function
#'
#' Compares player scores to dealer's and announces outcome.
#'
#' @param player_scores Named numeric vector of player scores.
#' @param dealer_score Numeric dealer score.
#' @return Named character vector of outcomes ("Win", "Lose", "Tie").
#' @examples
#' # Dealer has 18
#' dealer_score <- 18
#'
#' # Case 1: Player1 beats dealer, Player2 busts, Player3 ties
#' scores <- c(Player1 = 20, Player2 = 23, Player3 = 18)
#' announce_winner(scores, dealer_score)
#' # Expected output:
#' # Player1 "Win"
#' # Player2 "Lose"
#' # Player3 "Tie"
#'
#' # Case 2: Dealer busts, only players with <= 21 win
#' announce_winner(c(Player1 = 17, Player2 = 22), dealer_score = 25)
#' # Expected:
#' # Player1 "Win", Player2 "Lose"
#'
#' # Case 3: Dealer and all players bust
#' announce_winner(c(P1 = 22, P2 = 30), dealer_score = 26)
#' # Expected: both "Lose" (players bust regardless of dealer bust)
#'
#' @export

announce_winner <- function(player_scores, dealer_score) {
  # Ensure dealer_score is a single value
  if (length(dealer_score) > 1) {
    stop("dealer_score must be a single numeric value.")
  }

  outcomes <- sapply(player_scores, function(score) {
    if (score > 21) {
      "Lose"
    } else if (dealer_score > 21) {
      "Win"
    } else if (score > dealer_score) {
      "Win"
    } else if (score == dealer_score) {
      "Tie"
    } else {
      "Lose"
    }
  })
  return(outcomes)
}
