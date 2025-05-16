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
#' @export
announce_winner <- function(player_scores, dealer_score) {
  outcomes <- sapply(player_scores, function(score) {
    if (score > 21) "Lose"
    else if (dealer_score > 21) "Win"
    else if (score > dealer_score) "Win"
    else if (score == dealer_score) "Tie"
    else "Lose"
  })
  return(outcomes)
}
