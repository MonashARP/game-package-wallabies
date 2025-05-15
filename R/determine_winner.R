#' Announce Winner Function
#'
#' This function announces the winner(s) of the game based on hand scores.
#'
#' @param player_scores A named vector of scores for each player.
#' @param dealer_score The dealer's score.
#' @return A named vector indicating the outcome for each player (Win, Lose, Tie).
#' @examples
#' announce_winner(c(Player1 = 21, Player2 = 18), 20) # Player1 wins, Player2 loses.
#' announce_winner(c(Player1 = 19, Player2 = 18), 19) # Player1 ties, Player2 loses.
#' @export

announce_winner <- function(player_scores, dealer_score) {
  outcomes <- sapply(player_scores, function(player_score) {
    if (player_score > 21) {
      return("Lose")  # Player busted
    } else if (dealer_score > 21) {
      return("Win")  # Dealer busted
    } else if (player_score > dealer_score) {
      return("Win")
    } else if (player_score == dealer_score) {
      return("Tie")
    } else {
      return("Lose")
    }
  })

  return(outcomes)
}
