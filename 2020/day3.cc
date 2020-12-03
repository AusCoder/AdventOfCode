#include "bits-and-bobs.hh"

int countSlope(const std::vector<std::string> &forest, int shiftX, int shiftY) {
  int width = forest.at(0).size();
  int height = forest.size();
  int x = 0;
  int y = 0;
  int count = 0;
  while (y < height) {
    if (forest.at(y).at(x) == '#') {
      count++;
    }
    x = (x + shiftX) % width;
    y = y + shiftY;
  }
  return count;
}

void part1(const std::vector<std::string> &forest) {
  auto count = countSlope(forest, 3, 1);
  print(count);
}

void part2(const std::vector<std::string> &forest) {
  std::vector<std::vector<int>> combs = {
      {1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}};
  auto product = std::accumulate(
      combs.cbegin(), combs.cend(), 1L, [=](auto acc, auto comb) {
        return acc * countSlope(forest, comb.at(0), comb.at(1));
      });
  print(product);
}

int main() {
  std::ifstream fInput{"input/day3.txt"};
  assert(fInput.is_open());
  auto forest = readLines(fInput);
  part1(forest);
  part2(forest);
}
