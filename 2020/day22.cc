#include "bits-and-bobs.hh"

using namespace std;

struct Decks {
  deque<int> player1;
  deque<int> player2;
};

inline bool operator==(const Decks &d1, const Decks &d2) {
  bool player1Match =
      (d1.player1.size() == d2.player1.size()) &&
      std::equal(d1.player1.cbegin(), d1.player1.cend(), d2.player1.cbegin());
  bool player2Match =
      (d1.player2.size() == d2.player2.size()) &&
      std::equal(d1.player2.cbegin(), d1.player2.cend(), d2.player2.cbegin());
  return player1Match && player2Match;
}

Decks parseDecks(const vector<string> &lines) {
  deque<int> player1;
  auto it = lines.cbegin();
  assert(it++->starts_with("Player"));
  while (!it->empty()) {
    player1.push_back(std::stoi(*it++));
  }
  it++;
  deque<int> player2;
  assert(it++->starts_with("Player"));
  while (!it->empty()) {
    player2.push_back(std::stoi(*it++));
  }
  return {std::move(player1), std::move(player2)};
}

long calculateScore(const Decks &decks) {
  // calculate score
  int mult = 1;
  long score = 0;
  const auto &winner = !decks.player1.empty() ? decks.player1 : decks.player2;
  for (auto it = winner.rbegin(); it != winner.rend(); it++) {
    score += mult++ * *it;
  }
  return score;
}

void part1(const vector<string> &lines) {
  Decks decks = parseDecks(lines);
  // play the game
  while (!(decks.player1.empty() || decks.player2.empty())) {
    if (decks.player1.front() > decks.player2.front()) {
      decks.player1.push_back(decks.player1.front());
      decks.player1.push_back(decks.player2.front());
    } else if (decks.player1.front() < decks.player2.front()) {
      decks.player2.push_back(decks.player2.front());
      decks.player2.push_back(decks.player1.front());
    } else {
      assert(false);
    }
  }
  // calculate score
  print(calculateScore(decks));
}

struct Game {
  Decks decks;
  // some kind of hash or memory thing to remember previous
  // rounds of this game.
  vector<Decks> previousRounds;

  void saveCurrentDeck() { previousRounds.push_back(decks); }

  bool seenCurrentDeckBefore() const {
    return std::any_of(
        previousRounds.cbegin(), previousRounds.cend(),
        [this](const auto &previousDecks) { return previousDecks == decks; });
  }
};

bool calculateIsPlayer1Winner(Game &game) {
  Decks &decks = game.decks;
  while (!(decks.player1.empty() || decks.player2.empty())) {
    // some check if we have seen this state before
    if (game.seenCurrentDeckBefore()) {
      return true;
    }
    // add new state to game state cache
    game.saveCurrentDeck();
    // play the game
    int player1Card = decks.player1.front();
    int player2Card = decks.player2.front();
    decks.player1.pop_front();
    decks.player2.pop_front();
    int player1Size = decks.player1.size();
    int player2Size = decks.player2.size();

    bool player1IsWinner = true;
    if ((player1Size >= player1Card) && (player2Size >= player2Card)) {
      // new game
      deque<int> newPlayer1Deck;
      std::copy_n(decks.player1.cbegin(), player1Card,
                  std::back_inserter(newPlayer1Deck));
      deque<int> newPlayer2Deck;
      std::copy_n(decks.player2.cbegin(), player2Card,
                  std::back_inserter(newPlayer2Deck));

      // Caching the results using initial game state
      // doesn't seem to be needed
      Game newGame{{std::move(newPlayer1Deck), std::move(newPlayer2Deck)}};
      player1IsWinner = calculateIsPlayer1Winner(newGame);
    } else if (player1Card > player2Card) {
      player1IsWinner = true;
    } else if (player1Card < player2Card) {
      player1IsWinner = false;
    } else {
      assert(false);
    }

    if (player1IsWinner) {
      decks.player1.push_back(player1Card);
      decks.player1.push_back(player2Card);
    } else {
      decks.player2.push_back(player2Card);
      decks.player2.push_back(player1Card);
    }
  }
  return !decks.player1.empty();
}

void part2(const vector<string> &lines) {
  Decks decks = parseDecks(lines);
  Game game{decks};
  calculateIsPlayer1Winner(game);
  print(calculateScore(game.decks));
}

int main() {
  auto lines = readLinesFromFile("input/day22.txt");
  part1(lines);
  part2(lines);
}
