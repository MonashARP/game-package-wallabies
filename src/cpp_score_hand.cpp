#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int cpp_score_hand(CharacterVector hand) {
  std::map<std::string, int> values = {
    {"A", 11}, {"2", 2}, {"3", 3}, {"4", 4}, {"5", 5},
    {"6", 6}, {"7", 7}, {"8", 8}, {"9", 9},
    {"10", 10}, {"J", 10}, {"Q", 10}, {"K", 10}
  };

  int score = 0;
  int aces = 0;

  for (int i = 0; i < hand.size(); ++i) {
    std::string card = as<std::string>(hand[i]);
    std::string rank = card.substr(0, card.find_first_of("♠♥♦♣"));

    if (values.find(rank) == values.end()) {
      stop("Invalid card rank: " + rank);
    }

    score += values[rank];
    if (rank == "A") ++aces;
  }

  while (score > 21 && aces > 0) {
    score -= 10;
    --aces;
  }

  return score;
}
