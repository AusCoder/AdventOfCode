#include "bits-and-bobs.hh"

#define TARGET 2020

void part1(const std::vector<int> &nums) {
  for (int i = 0; i < nums.size(); i++) {
    for (int j = i + 1; j < nums.size(); j++) {
      if (nums.at(i) + nums.at(j) == TARGET) {
        std::cout << nums.at(i) * nums.at(j) << "\n";
      }
    }
  }
}

void part2(const std::vector<int> &nums) {
  for (int i = 0; i < nums.size(); i++) {
    for (int j = i + 1; j < nums.size(); j++) {
      for (int k = j + 1; k < nums.size(); k++) {
        if (nums.at(i) + nums.at(j) + nums.at(k) == TARGET) {
          std::cout << nums.at(i) * nums.at(j) * nums.at(k) << "\n";
        }
      }
    }
  }
}

int main() {
  std::string inputFileName = "input/day1.txt";
  std::ifstream fInput{inputFileName};
  assert(fInput.is_open());
  auto nums = pickColumn(readMatrixInt(fInput), 0);
  part1(nums);
  part2(nums);
}
