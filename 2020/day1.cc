#include "bits-and-bobs.hh"

#define TARGET 2020

void part1(const std::vector<int> &nums) {
  for (std::size_t i = 0; i < nums.size(); i++) {
    for (std::size_t j = i + 1; j < nums.size(); j++) {
      if (nums.at(i) + nums.at(j) == TARGET) {
        std::cout << nums.at(i) * nums.at(j) << "\n";
      }
    }
  }
}

void part2(const std::vector<int> &nums) {
  for (std::size_t i = 0; i < nums.size(); i++) {
    for (std::size_t j = i + 1; j < nums.size(); j++) {
      for (std::size_t k = j + 1; k < nums.size(); k++) {
        if (nums.at(i) + nums.at(j) + nums.at(k) == TARGET) {
          std::cout << nums.at(i) * nums.at(j) * nums.at(k) << "\n";
        }
      }
    }
  }
}

int main() {
  std::ifstream fInput{"input/day1.txt"};
  assert(fInput.is_open());
  auto nums = getColumn(0, readMatrixInt(fInput));
  part1(nums);
  part2(nums);
}
