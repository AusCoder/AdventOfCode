#include "bits-and-bobs.hh"

struct PasswordWithPolicy {
  int firstNum;
  int secondNum;
  char policyChar;
  std::string password;
};

PasswordWithPolicy
parsePasswordWithPolicy(const std::string &passwordWithPolicy) {
  std::regex r("(\\d+)-(\\d+)\\s+([a-zA-Z]):\\s+([a-zA-Z0-9]+)");
  std::smatch match;
  std::regex_match(passwordWithPolicy, match, r);
  assert(match.ready());
  if (match.size() != 5) {
    throw std::runtime_error(std::string("Failed to match on: ") +
                             passwordWithPolicy);
  }

  int firstNum = std::stoi(match.str(1));
  int secondNum = std::stoi(match.str(2));
  assert(firstNum <= secondNum);
  char policyChar = match.str(3)[0];
  std::string password = match.str(4);
  return {firstNum, secondNum, policyChar, password};
}

bool part1IsValid(const PasswordWithPolicy &passwordWithPolicy) {
  int count = 0;
  const std::string &password = passwordWithPolicy.password;
  std::for_each(password.cbegin(), password.cend(), [&](auto c) {
    if (c == passwordWithPolicy.policyChar) {
      count++;
    }
  });
  return (passwordWithPolicy.firstNum <= count) &&
         (count <= passwordWithPolicy.secondNum);
}

bool part2IsValid(const PasswordWithPolicy &passwordWithPolicy) {
  char c1 = passwordWithPolicy.password.at(passwordWithPolicy.firstNum - 1);
  char c2 = passwordWithPolicy.password.at(passwordWithPolicy.secondNum - 1);
  return ((c1 == passwordWithPolicy.policyChar) &&
          (c2 != passwordWithPolicy.policyChar)) ||
         ((c1 != passwordWithPolicy.policyChar) &&
          (c2 == passwordWithPolicy.policyChar));
}

template <typename F>
void printPredicateCount(const std::vector<std::string> &lines, F predicate) {
  int count = 0;
  std::for_each(lines.cbegin(), lines.cend(), [&](auto line) {
    auto passwordWithPolicy = parsePasswordWithPolicy(line);
    if (predicate(passwordWithPolicy)) {
      count++;
    }
  });
  std::cout << count << "\n";
}

int main() {
  auto passwordsWithPolicy = readLinesFromFile("input/day2.txt");
  printPredicateCount<decltype(part1IsValid)>(passwordsWithPolicy,
                                              part1IsValid);
  printPredicateCount<decltype(part1IsValid)>(passwordsWithPolicy,
                                              part2IsValid);
  return 0;
}
