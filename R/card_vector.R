#' Card Vector Constructor
#'
#' Creates a custom vctrs card vector representing a deck of cards.
#' Each card is represented as a string like "A♠", "10♣", "K♥", etc.
#'
#' @param x A character vector of cards, e.g., "A♠", "10♣", "K♥"
#' @return A `card_vector` object with class `"card_vector"`, inheriting from `"vctrs_vctr"`
#' @import vctrs
#'
#' @examples
#' # Create a card vector with a few cards
#' deck <- card_vector(c("A♠", "10♣", "K♥"))
#' deck
#' @export
card_vector <- function(x = character()) {
  vctrs::vec_assert(x, character())  # Ensure that input is a character vector
  structure(x, class = c("card_vector", "vctrs_vctr"))  # Create the custom card vector
}

#' Format Method for the `card_vector` Class
#'
#' Customizes the print output for `card_vector` objects by enclosing each card in brackets.
#'
#' @param x A `card_vector` object.
#' @param ... Additional arguments passed to `format()`.
#' @return A formatted character string representing the card vector.
#' @export
#' @examples
#' # Format the card vector
#' deck <- card_vector(c("A♠", "10♣", "K♥"))
#' format(deck)
format.card_vector <- function(x, ...) {
  paste0("[", x, "]")  # Wrap each card in square brackets
}

#' Type Coercion Method for `card_vector`
#'
#' Type coercion method for `card_vector` to ensure compatibility with other `card_vector` objects.
#'
#' @param x, y Objects to compare for type compatibility.
#' @param ... Additional arguments for method dispatch.
#' @return A `card_vector` object.
#'
#' @examples
#' # Type coercion method in action
#' deck1 <- card_vector(c("A♠", "10♣"))
#' deck2 <- card_vector(c("K♥", "Q♠"))
#' vec_ptype2(deck1, deck2)
#' @export
vec_ptype2.card_vector.card_vector <- function(x, y, ...) {
  card_vector()  # Return a `card_vector` type when comparing two card_vectors
}

#' Coercion Method for `card_vector`
#'
#' Coercion method for casting one `card_vector` to another.
#'
#' @param x A `card_vector` object.
#' @param to The target class to cast to.
#' @param ... Additional arguments for method dispatch.
#' @return The original `card_vector` object, as no conversion is needed.
#'
#'
#' @examples
#' # Coerce between two card_vectors
#' deck1 <- card_vector(c("A♠", "10♣"))
#' deck2 <- vec_cast(deck1, to = "card_vector")
#' deck2
#' @export
vec_cast.card_vector.card_vector <- function(x, to, ...) {
  x  # Return the original object as no casting is required between the same class
}

#' Get Card Ranks
#'
#' Extracts the rank (number or face value) of each card in the `card_vector`.
#'
#' @param x A `card_vector` object.
#' @return A character vector containing the ranks of the cards (e.g., "A", "10", "Q").
#'
#' @examples
#' # Extract card ranks from a deck
#' deck <- card_vector(c("A♠", "10♣", "K♥"))
#' card_rank(deck)
#' @export
card_rank <- function(x) {
  stopifnot(inherits(x, "card_vector"))  # Ensure input is a valid card_vector
  ranks <- vctrs::vec_data(x)  # Extract the underlying character data
  sub("[\u2660\u2663\u2665\u2666]", "", ranks)  # Remove suit symbols to isolate ranks
}

#' Get Card Suits
#'
#' Extracts the suit (♠, ♥, ♦, ♣) of each card in the `card_vector`.
#'
#' @param x A `card_vector` object.
#' @return A character vector containing the suits of the cards (e.g., "♠", "♥").
#'
#' @examples
#' # Extract card suits from a deck
#' deck <- card_vector(c("A♠", "10♣", "K♥"))
#' card_suit(deck)
#' @export
card_suit <- function(x) {
  stopifnot(inherits(x, "card_vector"))  # Ensure input is a valid card_vector
  ranks <- vctrs::vec_data(x)  # Extract the underlying character data
  gsub("[0-9JQKA]+", "", ranks)  # Remove the rank symbols (numbers, J, Q, K, A) to isolate suits
}

#' Check if a Card is a Face Card (J, Q, K)
#'
#' Identifies whether each card in the `card_vector` is a face card (Jack, Queen, or King).
#'
#' @param x A `card_vector` object.
#' @return A logical vector indicating whether each card is a face card (TRUE) or not (FALSE).
#'
#' @examples
#' # Check for face cards in a deck
#' deck <- card_vector(c("A♠", "10♣", "K♥", "Q♠"))
#' card_is_face(deck)
#' @export
card_is_face <- function(x) {
  rank <- card_rank(x)  # Extract card ranks
  rank %in% c("J", "Q", "K")  # Return TRUE if the rank is J, Q, or K
}
