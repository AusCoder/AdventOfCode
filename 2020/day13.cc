#include "bits-and-bobs.hh"

using namespace std;

pair<int, int> calculateDiffWithId(int arriveTime, int busId) {
  int div = arriveTime / busId;
  int diff = 0;
  if (div != 0) {
    diff = (div + 1) * busId - arriveTime;
  }
  return {diff, busId};
}

void part1(const vector<string> &lines) {
  int arriveTime = std::stoi(lines.at(0));
  auto parts = splitString(lines.at(1), ",");
  vector<string> busIdsStr;
  copy_if(parts.cbegin(), parts.cend(), back_inserter(busIdsStr),
          [](const auto &s) { return s != "x"; });
  vector<int> busIds(busIdsStr.size());
  transform(busIdsStr.cbegin(), busIdsStr.cend(), busIds.begin(),
            [](const auto &s) { return stoi(s); });

  vector<pair<int, int>> diffsWithId;
  transform(busIds.cbegin(), busIds.cend(), back_inserter(diffsWithId),
            [&](auto busId) { return calculateDiffWithId(arriveTime, busId); });
  auto closestDiffWithId =
      min_element(diffsWithId.cbegin(), diffsWithId.cend(),
                  [](auto &p1, auto &p2) { return p1.first < p2.first; });
  assert(closestDiffWithId != diffsWithId.cend());
  print(closestDiffWithId->first * closestDiffWithId->second);
}

pair<long, long> solveDiff(int b1, int b2, int offset, long initX, long initY) {
  long x = initX, y = initY;
  for (;;) {
    long sum = b1 * x - b2 * y + offset;
    if (sum == 0) {
      return {x, y};
    } else if (sum < 0) {
      x++;
    } else {
      y++;
    }
  }
}

bool isSolution(int b1, int b2, int offset, long n1) {
  return (b1 * n1 + offset) % b2 == 0;
}

void part2(const vector<string> &lines) {
  vector<pair<int, int>> busIdWithOffset;
  auto parts = splitString(lines.at(1), ",");
  for (size_t idx = 0; idx < parts.size(); idx++) {
    if (parts.at(idx) != "x") {
      busIdWithOffset.push_back({stoi(parts.at(idx)), idx});
    }
  }
  // const auto &id0 = busIdWithOffset.at(0);

  // Idea:
  // Something like this:
  // but keep running up

  // int minT = 0;
  // for (size_t idx = 1; idx < busIdWithOffset.size(); idx++) {
  //   const auto &id = busIdWithOffset.at(idx);
  //   auto solution = solveDiff(id0.first, id.first, id.second - id0.second,
  //                             (minT - id0.second) / id0.first, 0);
  //   minT = id0.first * solution.first + id0.second;
  //   cout << "solution: " << solution.first << ", " << solution.second <<
  //   "\n"; for (size_t jdx = 1; jdx < idx; jdx++) {
  //     const auto &idAtJdx = busIdWithOffset.at(jdx);
  //     cout << "solves 0," << jdx << ": "
  //          << isSolution(id0.first, idAtJdx.first, idAtJdx.second -
  //          id0.second,
  //                        solution.first)
  //          << "\n";
  //   }
  // }

  // Idea: keep searching the largest pair
  // auto largestIdPtr = max_element(
  //     busIdWithOffset.cbegin(), busIdWithOffset.cend(),
  //     [](const auto &p1, const auto &p2) { return p1.first < p2.first; });
  //     const auto &largest

  long curT = 0;
  int curIdIdx = 1;
  for (;;) {
    print(curT);
    const auto &id = busIdWithOffset.at(curIdIdx);
    auto solution = solveDiff(id0.first, id.first, id.second - id0.second,
                              (curT - id0.second) / id0.first,
                              (curT - id.second) / id.first);

    if (id0.first * solution.first + id0.second > curT) {
      curT = id0.first * solution.first + id0.second;
      bool solvesAll = true;
      for (size_t i = 1; i < busIdWithOffset.size(); i++) {
        const auto &idToCheck = busIdWithOffset.at(i);
        solvesAll = solvesAll &&
                    isSolution(id0.first, idToCheck.first,
                               idToCheck.second - id0.second, solution.first);
      }
      if (solvesAll) {
        print(curT);
        break;
      }
    }

    curIdIdx++;
    curIdIdx = curIdIdx >= busIdWithOffset.size() ? 1 : curIdIdx;
  }
}

int main() {
  auto lines = readLinesFromFile("input/day13.txt");
  // part1(lines);
  part2(lines);
}
