#' Score the Cards in a Hand with Dynamic Rules
#'
#' This function calculates the score of a hand based on dynamic rules.
#' By default, it uses the standard Blackjack rules, but it can be customized to
#' use other rules, such as always treating Aces as 1, or applying custom scoring mechanisms.
#'
#' @param hand A vector of cards.
#' @param rule A character string indicating the scoring rule to use.
#'   Options: "standard" (default), "custom". See details below.
#' @return The score of the hand.
#' @examples
#' # Score a hand with Ace and 9 (standard rule)
#' score_hand_dynamic(c("A♠", "9♣"))
#'
#' # Score a hand with multiple Aces using the custom rule (Ace is always 1)
#' score_hand_dynamic(c("A♠", "A♦", "9♠"), rule = "custom")
#'
#' # Score a hand with multiple Aces and a 10, resulting in a bust (standard rule)
#' score_hand_dynamic(c("A♠", "A♦", "A♣", "10♠"))
#'
#' @export
score_hand_dynamic <- function(hand, rule = "standard") {
  # Define the values for each rank
  values <- c("A" = 11, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10, "J" = 10, "Q" = 10, "K" = 10)

  # Extract ranks from the hand (remove suits)
  ranks <- sapply(hand, function(card) {
    rank <- strsplit(card, "[♠♦♥♣]")[[1]][1]
    return(rank)
  })

  # Adjust scores based on rule
  if (rule == "standard") {
    # Standard rule: Ace is 11 unless the total score is over 21, in which case Ace becomes 1
    score <- sum(values[ranks])
    aces <- sum(ranks == "A")
    while (score > 21 && aces > 0) {
      score <- score - 10  # Adjust Ace from 11 to 1
      aces <- aces - 1
    }
  } else if (rule == "custom") {
    # Custom rule: Ace is always worth 1, no matter the total score
    ranks[ranks == "A"] <- "1"  # Replace Ace with "1"
    values["1"] <- 1  # Define "1" to be worth 1
    score <- sum(values[ranks])  # Sum the values using the updated ranks
  } else {
    stop("Unknown rule. Use 'standard' or 'custom'.")
  }

  return(score)
}
