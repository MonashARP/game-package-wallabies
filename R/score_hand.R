#' Score the Cards in a Hand
#'
#' @param hand A vector of cards.
#' @return The score of the hand.
#' @examples
#' # Score a hand with Ace and 9
#' score_hand(c("A♠", "9♣"))
#'
#' # Score a hand with multiple Aces
#' score_hand(c("A♠", "A♦", "9♠"))
#'
#' # Score a hand with multiple Aces and a 10, resulting in a bust
#' score_hand(c("A♠", "A♦", "A♣", "10♠"))

#' @export

score_hand <- function(hand) {
  # Define the values for each rank
  values <- c("A" = 11, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10, "J" = 10, "Q" = 10, "K" = 10)

  # Extract the rank part of each card (e.g., "A" from "A♠", "10" from "10♥")
  ranks <- sapply(hand, function(card) {
    # Split the card string at any non-numeric character (i.e., the suit)
    rank <- strsplit(card, "[♠♦♥♣]")[[1]][1]
    return(rank)
  })

  # Get the values of the cards based on the rank
  card_values <- values[ranks]

  # Check if there are any NA values in card_values (this indicates that the rank was not found)
  if (any(is.na(card_values))) {
    stop("Invalid card rank detected.")
  }

  # Calculate the total score
  score <- sum(card_values)

  # Adjust for Aces: If score > 21 and there's an Ace, adjust Ace to be worth 1
  aces <- sum(ranks == "A")
  while (score > 21 && aces > 0) {
    score <- score - 10  # Adjust Ace from 11 to 1
    aces <- aces - 1
  }

  return(score)
}

