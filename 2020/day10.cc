#include "bits-and-bobs.hh"

using namespace std;

void part1(const vector<int> &ratings) {
  vector<int> joltRatings{ratings.begin(), ratings.end()};
  sort(joltRatings.begin(), joltRatings.end());
  unordered_map<int, int> diffCounts;
  for (auto it = joltRatings.begin(); it != joltRatings.end() - 1; it++) {
    int diff = *(it + 1) - *it;
    if (diffCounts.find(diff) == diffCounts.end()) {
      diffCounts.insert({diff, 0});
    }
    diffCounts.at(diff)++;
  }
  // outlet diff
  diffCounts.at(joltRatings.at(0))++;
  // device diff
  diffCounts.at(3)++;
  print(diffCounts.at(1) * diffCounts.at(3));
}

long countUpToPos(unordered_map<int, long> &cache,
                  const vector<int> &sortedJoltRatings, int pos) {
  if (cache.find(pos) != cache.end()) {
    return cache.at(pos);
  }
  long count = 0;
  if (sortedJoltRatings.at(pos) <= 3) {
    count++;
  }
  int n = 1;
  while ((pos - n >= 0) &&
         (sortedJoltRatings.at(pos) - sortedJoltRatings.at(pos - n) <= 3)) {
    count += countUpToPos(cache, sortedJoltRatings, pos - n);
    n++;
  }
  cache.insert({pos, count});
  return count;
}

/*
  Sort jolts.

  There is a recursion:

  countUpToPos(n) = \sum_i if (diff <= 3) { countUpToPos(n - i) }
  where i >= 1

  Example for beginning:

  (0) 1 2 3
  pos = 0 -> 1
  pos = 1 -> 1 + countUpToPos(0) = 2
  pos = 2 -> 1 + countUpToPos(1) + countUpToPos(0) = 4

  The final device adapter has no contribution to count
  because diff is 3, so only 1 way.
*/
void part2(const vector<int> &ratings) {
  vector<int> joltRatings{ratings.begin(), ratings.end()};
  sort(joltRatings.begin(), joltRatings.end());
  unordered_map<int, long> cache;
  auto count = countUpToPos(cache, joltRatings, joltRatings.size() - 1);
  print(count);
}

int main() {
  auto lines = readLinesFromFile("input/day10.txt");
  auto joltRatings = getColumn(0, parseMatrixInt(lines));
  part1(joltRatings);
  part2(joltRatings);
}
