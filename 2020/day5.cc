#include "bits-and-bobs.hh"

int calculateSeatId(const std::string &boardingPass) {
  assert(boardingPass.size() == 10);
  int seatId = 0;
  for (std::size_t i = 0; i < 10; i++) {
    if ((boardingPass.at(i) == 'B') || (boardingPass.at(i) == 'R')) {
      seatId += (1 << (10 - 1 - i));
    }
  }
  return seatId;
}

void part1(const std::vector<std::string> &lines) {
  std::vector<int> seatIds;
  std::transform(lines.cbegin(), lines.cend(), std::back_inserter(seatIds),
                 calculateSeatId);
  int m = *std::max_element(seatIds.cbegin(), seatIds.cend());
  print(m);
}

void part2(const std::vector<std::string> &lines) {
  std::array<int, 1 << 10> seats;
  std::fill(seats.begin(), seats.end(), 0);
  std::for_each(lines.cbegin(), lines.cend(),
                [&](const auto &line) { seats.at(calculateSeatId(line)) = 1; });
  for (std::size_t i = 0; i < seats.size() - 2; i++) {
    if ((seats.at(i) == 1) && (seats.at(i + 1) == 0) &&
        (seats.at(i + 2) == 1)) {
      print(i + 1);
    }
  }
}

int main() {
  auto lines = readLinesFromFile("input/day5.txt");
  part1(lines);
  part2(lines);
}
