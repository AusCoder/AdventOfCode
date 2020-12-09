#include "bits-and-bobs.hh"
using namespace std;

bool isValid(const vector<long> &sortedPreviousNumbers, long number) {
  auto itFront = sortedPreviousNumbers.cbegin();
  auto itBack = sortedPreviousNumbers.cend() - 1;
  while (itFront < itBack) {
    auto curSum = *itFront + *itBack - number;
    if (curSum == 0) {
      return true;
    } else if (curSum < 0) {
      itFront++;
    } else {
      itBack--;
    }
  }
  return false;
}

long findInvalidNumber(int preambleSize, const vector<long> &numbers) {
  vector<long> preamble{numbers.cbegin(), numbers.cbegin() + preambleSize};
  std::sort(preamble.begin(), preamble.end());

  for (auto it = numbers.cbegin() + preambleSize; it != numbers.end(); it++) {
    if (isValid(preamble, *it)) {
      auto pos = find(preamble.begin(), preamble.end(), *(it - preambleSize));
      preamble.at(pos - preamble.cbegin()) = *it;
      std::sort(preamble.begin(), preamble.end());
    } else {
      return *it;
    }
  }
  throw std::runtime_error("Didn't find number");
}

void part1(int preambleSize, const vector<long> &numbers) {
  print(findInvalidNumber(preambleSize, numbers));
}

void part2(int preambleSize, const vector<long> &numbers) {
  auto targetNumber = findInvalidNumber(preambleSize, numbers);
  for (auto it = numbers.cbegin(); it < numbers.cend(); it++) {
    // Need some kind of accumulate while
    long acc = 0;
    int n = 0;
    while ((acc < targetNumber) && (it + n < numbers.cend())) {
      acc += *(it + n);
      n++;
    }

    if ((n > 1) && (acc == targetNumber)) {
      auto maxNum = *std::max_element(it, it + n);
      auto minNum = *std::min_element(it, it + n);
      print(maxNum + minNum);
      return;
    }
  }
  throw std::runtime_error("Didn't find target number");
}

int main() {
  auto lines = readLinesFromFile("input/day9.txt");
  auto numbers = getColumn(0, parseMatrixLong(lines));
  part1(25, numbers);
  part2(25, numbers);
}
