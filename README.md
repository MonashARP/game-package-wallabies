# blackjackGame 🂡🂱

An R package to simulate Blackjack including dealing, scoring, player options (split, double down), dealer logic, and insurance.

## Installation

```r
# install.packages("devtools")
devtools::install_github("your-username/blackjackGame")
```

## Example game flow

```r
library(blackjackGame)

deck <- create_board()
deal <- deal_cards(1, deck)
player <- unlist(deal$player_hands[[1]])
dealer <- deal$dealer_hand

score_hand(player)
score_hand(dealer)

insurance_bet(dealer, player_accepts = TRUE)

announce_winner(c(P1 = score_hand(player)), score_hand(dealer))
```

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

### Due Dates (uploaded by professor)

[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/7yy0sjQL)
