#include "bits-and-bobs.hh"

using namespace std;

template <typename T> void rotateFrontToBack(deque<T> &d) {
  d.push_back(d.front());
  d.pop_front();
}

void part1() {
  deque<int> cups{3, 2, 4, 1, 5};
  auto it = cups.cbegin();
  // play a round
  int cur = *it;

  // printContainer(cups);
  // rotateFrontToBack(cups);
  // deque<
  // printContainer(cups);
}

int main() { part1(); }
