#' Offer and Resolve Insurance Bet
#'
#' If the dealer's upcard is an Ace, allows the player to take insurance.
#' Insurance pays 2:1 if the dealer has a Blackjack.
#'
#' @param dealer_hand A character vector of two cards (e.g., c("A♠", "10♦"))
#' @param player_accepts Logical, TRUE if player accepts insurance
#'
#' @return A list with insurance_available, insurance_paid, payout, blackjack
#'
#' @examples
#' insurance_bet(c("A♠", "10♣"), player_accepts = TRUE)
#' insurance_bet(c("A♠", "8♣"), player_accepts = TRUE)
#' insurance_bet(c("9♠", "10♣"), player_accepts = TRUE)
#'
#' @export

insurance_bet <- function(dealer_hand, player_accepts, rule = "standard") {
  # Validate input
  if (length(dealer_hand) != 2) {
    stop("Dealer must have two cards.")
  }

  # Get dealer upcard rank
  upcard_rank <- strsplit(dealer_hand[1], "[♠♦♥♣]")[[1]][1]

  result <- list(
    insurance_available = FALSE,
    insurance_paid = FALSE,
    payout = 0,
    blackjack = FALSE
  )

  # Only offer insurance if upcard is Ace
  if (upcard_rank != "A") {
    return(result)
  }

  result$insurance_available <- TRUE

  if (!player_accepts) {
    return(result)
  }

  result$insurance_paid <- TRUE

  # Determine if dealer has blackjack
  dealer_score <- score_hand_dynamic(dealer_hand, rule = rule)  # Use dynamic scoring rule
  if (dealer_score == 21) {
    result$blackjack <- TRUE
    result$payout <- 2  # 2:1 payout if dealer has blackjack
  } else {
    result$payout <- -1  # Lost insurance if dealer doesn't have blackjack
  }

  return(result)
}

