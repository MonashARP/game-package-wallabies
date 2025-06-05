#' Play a Full Game of Blackjack Interactively
#'
#' Asks user for number of players and decks, and plays a full round of Blackjack
#'
#' @export

play_blackjack <- function(rule = "standard") {
  cat("🎮 Welcome to Blackjack!\n")

  # Ask for players & decks
  num_players <- as.integer(readline("🎲 How many players? "))
  while (is.na(num_players) || num_players < 1) {
    num_players <- as.integer(readline("❌ Invalid. Enter at least 1 player: "))
  }

  num_decks_input <- readline("🃏 How many decks? (Enter for default): ")
  num_decks <- if (num_decks_input == "") ceiling(num_players / 2) else as.integer(num_decks_input)
  while (is.na(num_decks) || num_decks < 1) {
    num_decks_input <- readline("❌ Invalid. Enter valid number of decks: ")
    num_decks <- as.integer(num_decks_input)
  }

  deck <- create_board(num_decks = num_decks)
  deal <- deal_cards(num_players, deck)
  player_hands <- deal$player_hands
  dealer_hand <- deal$dealer_hand
  deck <- deal$deck

  cat("\n🤵 Dealer shows: ", dealer_hand, "\n\n")

  # Player scores initialized
  player_scores <- numeric(num_players)
  surrender_flags <- logical(num_players)
  insurance_results <- vector("list", num_players)

  # Add second card to dealer (for insurance check)
  dealer_hand <- c(dealer_hand, deck[1])
  deck <- deck[-1]

  # Offer insurance if dealer upcard is Ace
  upcard_rank <- strsplit(dealer_hand[1], "[♠♦♥♣]")[[1]][1]
  if (upcard_rank == "A") {
    for (i in 1:num_players) {
      pname <- paste0("Player", i)
      ans <- readline(paste0("  ➤ ", pname, ", dealer shows Ace. Take insurance? (y/n): "))
      insurance_results[[i]] <- insurance_bet(dealer_hand, tolower(ans) == "y")
      if (insurance_results[[i]]$insurance_paid) {
        if (insurance_results[[i]]$blackjack) {
          cat("  💰 ", pname, ": Dealer has Blackjack! Insurance paid 2:1.\n")
        } else {
          cat("  ❌ ", pname, ": Dealer has no Blackjack. Insurance lost.\n")
        }
      }
    }
  }

  # --- Each player's turn ---
  for (i in 1:num_players) {
    pname <- paste0("Player", i)
    hand <- player_hands[[i]]
    cat("👤", pname, "initial hand:", paste(hand, collapse = ", "), "\n")

    # Step 1: Ask Surrender
    ans <- readline("  ➤ Do you want to surrender? (y/n): ")
    if (tolower(ans) == "y") {
      surrender <- surrender_hand(hand, allow_surrender = TRUE)
      if (surrender$status == "surrendered") {
        cat("  🔁 ", surrender$message, "\n")
        surrender_flags[i] <- TRUE
        player_scores[i] <- 0
        next
      }
    }

    # Step 2: Check for Split
    split_check <- player_split(hand)
    if (split_check$can_split) {
      ans <- readline("  ➤ You have a pair. Split? (y/n): ")
      if (tolower(ans) == "y") {
        split_scores <- c()
        for (j in 1:2) {
          shand <- split_check$hands[[j]]
          cat("  🔀 Playing split hand", j, ":", paste(shand, collapse = ", "), "\n")
          repeat {
            score <- score_hand_dynamic(shand, rule = rule)  # Use score_hand_dynamic
            if (score >= 21 || length(shand) >= 5) break
            cat("    ➤ Hand", j, ":", paste(shand, collapse = ", "), "(", score, ")\n")
            ans <- readline("    ➤ Hit or stand? (h/s): ")
            while (!ans %in% c("h", "s")) {
              ans <- readline("    ❌ Invalid. Hit or stand? (h/s): ")
            }
            if (ans == "h") {
              shand <- c(shand, deck[1])
              deck <- deck[-1]
            } else {
              break
            }
          }
          final <- score_hand_dynamic(shand, rule = rule)  # Use score_hand_dynamic
          cat("    → Final score:", final, if (final > 21) "💥 BUST!" else "", "\n")
          split_scores <- c(split_scores, final)
        }
        valid_scores <- split_scores[split_scores <= 21]
        if (length(valid_scores) == 0) {
          player_scores[i] <- max(split_scores)
        } else {
          player_scores[i] <- max(valid_scores)
        }
        next
      }
    }

    # Step 3: Double Down
    if (length(hand) == 2 && score_hand_dynamic(hand, rule = rule) %in% c(10, 11)) {
      ans <- readline("  ➤ Double down? (y/n): ")
      if (tolower(ans) == "y") {
        dd <- double_down(hand, deck, rule = rule)
        hand <- dd$new_hand
        deck <- dd$deck
        if (dd$valid) {
          cat("  → Double down hand:", paste(hand, collapse = ", "), "\n")
          final_score <- score_hand_dynamic(hand, rule = rule)  # Use score_hand_dynamic
          cat("  → Final score:", final_score, if (final_score > 21) " 💥 BUST!" else "", "\n\n")
          player_scores[i] <- final_score
          next
        }
      }
    }

    # Step 4: Hit / Stand Loop
    repeat {
      score <- score_hand_dynamic(hand, rule = rule)  # Use score_hand_dynamic
      if (score >= 21 || length(hand) >= 5) break
      cat("  ➤ Current hand:", paste(hand, collapse = ", "), "(", score, ")\n")
      ans <- readline("  ➤ Hit or stand? (h/s): ")
      while (!ans %in% c("h", "s")) {
        ans <- readline("  ❌ Invalid. Hit or stand? (h/s): ")
      }
      if (ans == "h") {
        hand <- c(hand, deck[1])
        deck <- deck[-1]
      } else {
        break
      }
    }

    final_score <- score_hand_dynamic(hand, rule = rule)  # Use score_hand_dynamic
    cat("  → Final score:", final_score, if (final_score > 21) " 💥 BUST!" else "", "\n\n")
    player_scores[i] <- final_score
  }

  # --- Dealer plays ---
  cat("🤵 Dealer's full hand:", paste(dealer_hand, collapse = ", "), "\n")
  dealer_turn <- dealer_play(dealer_hand, deck, rule = rule)  # Pass rule to dealer play logic
  dealer_hand <- dealer_turn$dealer_hand
  dealer_score <- score_hand_dynamic(dealer_hand, rule = rule)  # Use score_hand_dynamic for dealer score
  cat("✅ Dealer's final hand:", paste(dealer_hand, collapse = ", "), "\n")
  cat("➡ Dealer score:", dealer_score, if (dealer_score > 21) " 💥 BUST!" else "", "\n\n")

  names(player_scores) <- paste0("Player", 1:length(player_scores))
  outcomes <- announce_winner(player_scores, dealer_score)  # Pass dealer_score as numeric

  for (i in seq_along(outcomes)) {
    if (i <= num_players && surrender_flags[i]) {
      cat("👤", names(outcomes)[i], ": ❌ Surrendered\n")
    } else {
      cat("👤", names(outcomes)[i], ":", outcomes[i], "\n")
    }
  }

  invisible(list(
    player_hands = player_hands,
    dealer_hand = dealer_hand,
    player_scores = player_scores,
    dealer_score = dealer_score,
    outcomes = outcomes,
    insurance = insurance_results
  ))
}

