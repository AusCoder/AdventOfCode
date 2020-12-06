#include "bits-and-bobs.hh"
#include <unordered_set>

void part1(const std::vector<std::string> &lines) {
  std::unordered_set<char> answers{};
  int count = 0;
  for (const auto &line : lines) {
    if (!line.empty()) {
      std::for_each(line.cbegin(), line.cend(),
                    [&](auto c) { answers.insert(c); });
    } else {
      count += answers.size();
      answers.clear();
    }
  }
  if (!answers.empty()) {
    count += answers.size();
  }
  print(count);
}

int countWhereEveroneAnswered(const std::unordered_map<char, int> &answers,
                              int numPeople) {
  return std::accumulate(answers.cbegin(), answers.cend(), 0,
                         [=](auto acc, auto pair) {
                           if (pair.second == numPeople) {
                             return acc + 1;
                           }
                           return acc;
                         });
}

void part2(const std::vector<std::string> &lines) {
  std::unordered_map<char, int> answers{};
  int numPeople = 0;
  int count = 0;
  for (const auto &line : lines) {
    if (!line.empty()) {
      numPeople++;
      std::for_each(line.cbegin(), line.cend(), [&](auto c) {
        if (answers.find(c) == answers.cend()) {
          answers.insert({c, 0});
        }
        answers.at(c)++;
      });
    } else {
      count += countWhereEveroneAnswered(answers, numPeople);
      numPeople = 0;
      answers.clear();
    }
  }
  if (!answers.empty()) {
    count += countWhereEveroneAnswered(answers, numPeople);
  }
  print(count);
}

int main() {
  auto lines = readLinesFromFile("input/day6.txt");
  part1(lines);
  part2(lines);
}
