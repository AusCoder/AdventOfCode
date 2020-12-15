#include "bits-and-bobs.hh"

using namespace std;

void playGame(const vector<string> &lines, int stepToPrint) {
  auto numbersStr = splitString(lines.at(0), ",");
  vector<int> startNumbers;
  transform(numbersStr.cbegin(), numbersStr.cend(), back_inserter(startNumbers),
            [](const auto &s) { return stoi(s); });

  unordered_map<int, int> lastSeenTurn;
  for (size_t i = 0; i < startNumbers.size() - 1; i++) {
    lastSeenTurn[startNumbers.at(i)] = i + 1;
  }

  int initNumbersSize = startNumbers.size();
  int lastTurnValue = startNumbers.at(startNumbers.size() - 1);
  for (int i = 0; i < stepToPrint - initNumbersSize; i++) {
    int turn = initNumbersSize + i + 1;
    int nextValue = 0;
    if (lastSeenTurn.find(lastTurnValue) != lastSeenTurn.end()) {
      nextValue = turn - 1 - lastSeenTurn.at(lastTurnValue);
    }
    lastSeenTurn[lastTurnValue] = turn - 1;
    lastTurnValue = nextValue;
  }
  print(lastTurnValue);
}

int main() {
  auto lines = readLinesFromFile("input/day15.txt");
  playGame(lines, 2020);
  playGame(lines, 30000000);
}
