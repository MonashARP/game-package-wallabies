

#' Card Vector Constructor
#'
#' Creates a custom vctrs card vector representing a deck of cards.
#' Each card is represented as a string like "A♠", "10♣", "K♥", etc.
#'
#' @param x A character vector of cards, e.g., "A♠", "10♣", "K♥"
#' @return A `card_vector` object with class `"card_vector"`, inheriting from `"vctrs_vctr"`
#' @import vctrs
#' @export
#' @examples
#' # Create a card vector with a few cards
#' deck <- card_vector(c("A♠", "10♣", "K♥"))
#' deck
card_vector <- function(x = character()) {
  if (inherits(x, "card_vector")) return(x)
  vctrs::vec_assert(x, character())  # Ensure input is a character vector
  structure(x, class = c("card_vector", "vctrs_vctr"))
}

#' Format Method for `card_vector`
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
  paste0("[", x, "]")
}

#' Type Coercion Method for `card_vector`
#'
#' Type coercion method for `card_vector` to ensure compatibility with other `card_vector` objects.
#'
#' @param x, y Objects to compare for type compatibility.
#' @param ... Additional arguments for method dispatch.
#' @return A `card_vector` object.
#' @export
#' @examples
#' # Type coercion method in action
#' deck1 <- card_vector(c("A♠", "10♣"))
#' deck2 <- card_vector(c("K♥", "Q♠"))
#' vctrs::vec_ptype2(deck1, deck2)
vec_ptype2.card_vector.card_vector <- function(x, y, ...) {
  card_vector()
}

#' Coercion Method for `card_vector`
#'
#' Coerces a `card_vector` object to another type (e.g., `character`).
#'
#' @param x A `card_vector` object.
#' @param to The target class to cast to.
#' @param ... Additional arguments for method dispatch.
#' @return The original `card_vector` object, or a converted object.
#' @export
#' @examples
#' # Coerce between two card_vectors
#' deck1 <- card_vector(c("A♠", "10♣"))
#' deck2 <- as.character(deck1)
#' deck3 <- vctrs::vec_cast(deck2, to = "character")
#' deck3
vec_cast.card_vector.card_vector <- function(x, to, ...) {
  if (inherits(x, "card_vector")) return(x)
  card_vector(as.character(x))
}

#' Coercion Method for `card_vector` to `character`
#'
#' This function converts a `card_vector` to a character vector by extracting
#' the underlying character data.
#'
#' @param x A `card_vector` object.
#' @param ... Additional arguments passed to the function.
#' @return A character vector.
#' @export
#' @examples
#' # Coerce card_vector to character
#' deck <- card_vector(c("A♠", "10♣"))
#' character_deck <- as.character(deck)
#' character_deck
as.character.card_vector <- function(x, ...) {
  vctrs::vec_data(x)  # Return the raw character vector from card_vector
}

#' Coercion Method for `card_vector` to `character` for later use
#'
#' Coercion method to cast a `card_vector` to a `character` vector.
#'
#' @param x A `card_vector` object.
#' @param to The target class to cast to (character).
#' @param ... Additional arguments for method dispatch.
#' @return A character vector.
#' @export
#' @examples
#' # Coerce card_vector to character
#' deck <- card_vector(c("A♠", "10♣"))
#' character_deck <- vctrs::vec_cast(deck, to = "character")
#' character_deck
vec_cast.card_vector.character <- function(x, to, ...) {
  as.character(x)
}

#' Coercion Method for `character` to `card_vector`
#'
#' Converts a `character` vector to a `card_vector`.
#'
#' @param x A `character` vector.
#' @param to The target class to cast to (`card_vector`).
#' @param ... Additional arguments for method dispatch.
#' @return A `card_vector` object.
#' @export
#' @examples
#' # Coerce character vector to card_vector
#' character_deck <- c("A♠", "10♣")
#' deck <- vctrs::vec_cast(character_deck, to = "card_vector")
#' deck
vec_cast.character.card_vector <- function(x, to, ...) {
  card_vector(x)  # Return as card_vector
}

#' Get Card Ranks
#'
#' Extracts the rank (number or face value) of each card in the `card_vector`.
#'
#' @param x A `card_vector` object.
#' @return A character vector containing the ranks of the cards.
#' @export
#' @examples
#' # Extract card ranks from a deck
#' deck <- card_vector(c("A♠", "10♣", "K♥"))
#' card_rank(deck)
card_rank <- function(x) {
  stopifnot(inherits(x, "card_vector"))
  ranks <- vctrs::vec_data(x)
  sub("[\u2660\u2663\u2665\u2666]", "", ranks)
}

#' Get Card Suits
#'
#' Extracts the suit of each card in the `card_vector`.
#'
#' @param x A `card_vector` object.
#' @return A character vector containing the suits of the cards.
#' @export
#' @examples
#' # Extract card suits from a deck
#' deck <- card_vector(c("A♠", "10♣", "K♥"))
#' card_suit(deck)
card_suit <- function(x) {
  stopifnot(inherits(x, "card_vector"))
  ranks <- vctrs::vec_data(x)
  gsub("[0-9JQKA]+", "", ranks)
}

#' Check if a Card is a Face Card (J, Q, K)
#'
#' Identifies whether each card in the `card_vector` is a face card (J, Q, K).
#'
#' @param x A `card_vector` object.
#' @return A logical vector indicating whether each card is a face card.
#' @export
#' @examples
#' # Check for face cards in a deck
#' deck <- card_vector(c("A♠", "10♣", "K♥", "Q♠"))
#' card_is_face(deck)
card_is_face <- function(x) {
  rank <- card_rank(x)
  rank %in% c("J", "Q", "K")
}

