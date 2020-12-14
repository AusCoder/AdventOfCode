#include "bits-and-bobs.hh"

using namespace std;

void part1(const vector<string> &lines) {
  std::regex maskRe("mask\\s=\\s([X01]+)");
  std::smatch maskMatch;
  std::regex_match(lines.at(0), maskMatch, maskRe);
  assert(!maskMatch.empty());
  string maskStr = maskMatch.str(1);
  print(maskStr);
  unsigned long andMask = 0;
  unsigned long orMask = 0;
  for (size_t i = 0; i < maskStr.size(); i++) {
    auto c = maskStr.at(i);
    auto shiftSize = maskStr.size() - 1 - i;
    print(shiftSize);
    if (c == 'X') {
      andMask |= (1 << shiftSize);
      // orMask &= (1 << shiftSize);
    } else if (c == '0') {
      // andMask already 0?
      // orMask &= (1 << shiftSize);
    } else if (c == '1') {
      orMask |= (1 << shiftSize);
      andMask |= (1 << shiftSize);
    } else {
      throw std::runtime_error("unknown char");
    }
  }
  print(andMask);
  print(orMask);
  print((11 & andMask) | orMask);

  // setting 1's is an Or mask: 0 -> 1, 1 -> 1
  // setting 0's is an And mask: 0 -> 0, 1 -> 0
}

int main() {
  auto lines = readLinesFromFile("input/day14.txt");
  part1(lines);
}
