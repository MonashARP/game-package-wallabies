#' Create a round_result object
#'
#' @param player Character: player name
#' @param hand Character vector: player's hand
#' @param score Integer: final score
#' @param outcome Character: outcome ("Win", "Lose", "Tie", "Surrender")
#'
#' @return An object of class `round_result`
#' @export
new_round_result <- function(player, hand, score, outcome) {
  structure(
    list(
      player = player,
      hand = hand,
      score = score,
      outcome = outcome
    ),
    class = "round_result"
  )
}

#' @export
format.round_result <- function(x, ...) {
  hand_str <- paste(x$hand, collapse = ", ")
  score_str <- paste0("(", x$score, if (x$score > 21) " 💥 BUST!" else "", ")")
  paste0("👤 ", x$player, ": ", hand_str, " ", score_str, " → ", x$outcome)
}

#' @export
print.round_result <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}
