#include "bits-and-bobs.hh"

using namespace std;

long transform(int loopSize, int subject) {
  long value = 1;
  for (int i = 0; i < loopSize; i++) {
    value *= subject;
    value %= 20201227;
    if (value < 0) {
      value += 20201227;
    }
  }
  return value;
}

int findLoopSize(int publicKey) {
  int value = 1;
  int subject = 7;
  int loopSize = 0;
  for (;;) {
    if (value == publicKey) {
      break;
    }
    loopSize++;
    value *= subject;
    value %= 20201227;
  }
  return loopSize;
}

void part1(const vector<string> &lines) {
  int cardPublicKey = std::stoi(lines.at(0));
  int doorPublicKey = std::stoi(lines.at(1));
  int cardLoopSize = findLoopSize(cardPublicKey);
  print(cardLoopSize);
  print(transform(cardLoopSize, doorPublicKey));
}

int main() {
  auto lines = readLinesFromFile("input/day25.txt");
  part1(lines);
}
