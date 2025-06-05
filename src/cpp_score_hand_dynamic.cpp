#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int cpp_score_hand_dynamic(CharacterVector hand, std::string rule = "standard") {
  std::map<std::string, int> values = {
    {"A", 11}, {"2", 2}, {"3", 3}, {"4", 4}, {"5", 5},
    {"6", 6}, {"7", 7}, {"8", 8}, {"9", 9},
    {"10", 10}, {"J", 10}, {"Q", 10}, {"K", 10}
  };

  std::vector<std::string> ranks;
  int score = 0;
  int aces = 0;

  // Extract ranks and calculate initial score
  for (int i = 0; i < hand.size(); ++i) {
    std::string card = Rcpp::as<std::string>(hand[i]);
    std::string rank = card.substr(0, card.find_first_of("♠♥♦♣"));
    if (values.find(rank) == values.end()) {
      Rcpp::stop("Invalid card rank: " + rank);
    }
    ranks.push_back(rank);
  }

  if (rule == "standard") {
    for (const auto& r : ranks) {
      score += values[r];
      if (r == "A") ++aces;
    }
    while (score > 21 && aces > 0) {
      score -= 10;
      --aces;
    }
  } else if (rule == "custom") {
    for (const auto& r : ranks) {
      if (r == "A") {
        score += 1;
      } else {
        score += values[r];
      }
    }
  } else {
    Rcpp::stop("Unknown rule. Use 'standard' or 'custom'.");
  }

  return score;
}
