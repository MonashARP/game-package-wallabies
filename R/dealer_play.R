#' Dealer's play logic according to Blackjack rules
#'
#' Dealer draws cards until reaching at least 17, or hitting 5 cards without busting.
#'
#' @param dealer_hand Character vector of the dealer's current hand
#' @param deck Character vector of the remaining deck
#'
#' @return A list with updated dealer_hand and deck
#' @examples
#' # Basic example
#' set.seed(123)
#' deck <- create_board(1)
#' dealer_hand <- c("A♠", "6♦")
#' result <- dealer_play(dealer_hand, deck)
#' print(result$dealer_hand)  # Should show hand with score >= 17
#'
#' # Dealer hits on soft 17: A♠ + 6♠
#' deck <- c("5♣", "2♦", "3♥", "K♠", "8♠", "9♣")  # sample deck
#' dealer_hand <- c("A♠", "6♠")
#' dealer_play(dealer_hand, deck)
#'
#' @export

dealer_play <- function(dealer_hand, deck, rule = "standard") {
  get_ace_count <- function(hand) {
    ranks <- sapply(hand, function(card) strsplit(card, "[♠♦♥♣]")[[1]][1])
    sum(ranks == "A")
  }

  while (length(dealer_hand) < 5) {
    score <- score_hand_dynamic(dealer_hand, rule)  # Use score_hand_dynamic instead of score_hand
    aces <- get_ace_count(dealer_hand)

    # Soft 17 check: has Ace and score == 17 before adjusting Ace
    is_soft_17 <- score == 17 && aces > 0

    if (score >= 17 && !is_soft_17) break

    # Draw a card
    dealer_hand <- c(dealer_hand, deck[1])
    deck <- deck[-1]
  }

  list(dealer_hand = dealer_hand, deck = deck)
}

