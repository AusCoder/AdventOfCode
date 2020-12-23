#include "bits-and-bobs.hh"

using namespace std;

template <typename T> void rotateFrontToBack(deque<T> &d) {
  d.push_back(d.front());
  d.pop_front();
}

void part1() {
  // deque<int> cups{3, 8, 9, 1, 2, 5, 4, 6, 7};
  deque<int> cups{6, 2, 4, 3, 9, 7, 1, 5, 8};
  int maxValue = *std::max_element(cups.cbegin(), cups.cend());
  int numRounds = 100;
  vector<int> removedValues(3);
  int current = cups.front();

  for (int i = 0; i < numRounds; i++) {
    // play a round
    // remove next 3 values
    auto removedValuesIt = removedValues.begin();
    auto nextPos = std::find(cups.cbegin(), cups.cend(), current) + 1;
    for (int i = 0; i < 3; i++) {
      if (nextPos == cups.cend()) {
        nextPos = cups.cbegin();
      }
      *removedValuesIt++ = *nextPos;
      nextPos = cups.erase(nextPos);
    }

    // find destination and insert
    int destinationValue = current >= 2 ? current - 1 : maxValue;
    while (std::find(removedValues.cbegin(), removedValues.cend(),
                     destinationValue) != removedValues.cend()) {
      destinationValue =
          destinationValue >= 2 ? destinationValue - 1 : maxValue;
    }
    auto insertPosition =
        std::find(cups.cbegin(), cups.cend(), destinationValue);
    assert(insertPosition != cups.cend());
    cups.insert(insertPosition + 1, removedValues.cbegin(),
                removedValues.cend());

    auto it = std::find(cups.cbegin(), cups.cend(), current) + 1;
    current = it != cups.cend() ? *it : cups.front();
  }
  // print values
  auto it = std::find(cups.cbegin(), cups.cend(), 1) + 1;
  if (it == cups.cend()) {
    it = cups.cbegin();
  }
  while (*it != 1) {
    cout << *it;
    if (++it == cups.cend()) {
      it = cups.cbegin();
    }
  }
  cout << "\n";
}

int main() { part1(); }
