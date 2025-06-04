// src/deck.cpp
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector create_deck_cpp() {
  // Define suits and ranks
  std::vector<std::string> suits  = {"♠", "♥", "♦", "♣"};
  std::vector<std::string> ranks  = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"};

  // Prepare an Rcpp CharacterVector of length 52
  CharacterVector deck(52);
  int index = 0;

  // Fill deck with "rank"+"suit", e.g. "A♠", "2♠", ..., "K♣"
  for (const auto &suit : suits) {
    for (const auto &rank : ranks) {
      deck[index++] = rank + suit;
    }
  }

  return deck;
}
