#include "bits-and-bobs.hh"

std::vector<std::map<std::string, std::string>>
parsePassports(const std::vector<std::string> &lines) {
  std::vector<std::map<std::string, std::string>> passports;
  std::map<std::string, std::string> fields = {};
  for (auto &line : lines) {
    if (line.size() == 0) {
      passports.push_back(fields);
      fields.clear();
    } else {
      std::istringstream iss{line};
      auto start = std::istream_iterator<std::string>(iss);
      auto end = std::istream_iterator<std::string>();
      while (start != end) {
        auto pos = start->find(':');
        auto key = start->substr(0, pos);
        auto value = start->substr(pos + 1, start->size());
        fields.insert({key, value});
        start++;
      }
    }
  }
  if (!fields.empty()) {
    passports.push_back(fields);
  }
  return passports;
}

bool part1IsValid(const std::map<std::string, std::string> &passport) {
  const std::vector<std::string> requiredFields = {"byr", "iyr", "eyr", "hgt",
                                                   "hcl", "ecl", "pid"};
  bool containsAllFields = true;
  for (const auto &req : requiredFields) {
    auto containsKey = std::accumulate(passport.cbegin(), passport.cend(),
                                       false, [=](auto acc, auto keyValue) {
                                         return acc || (req == keyValue.first);
                                       });
    containsAllFields = containsAllFields && containsKey;
  }
  return containsAllFields;
}

struct validator {
  virtual bool validate(const std::string &value) = 0;

  virtual ~validator() {}
};

struct byr_validator : public validator {
  bool validate(const std::string &value) override {
    if (isNumber(value)) {
      int num = std::stoi(value);
      return (num >= 1920) && (num <= 2002);
    }
    return false;
  }
};

struct iyr_validator : public validator {
  bool validate(const std::string &value) override {
    if (isNumber(value)) {
      int num = std::stoi(value);
      return (num >= 2010) && (num <= 2020);
    }
    return false;
  }
};

struct eyr_validator : public validator {
  bool validate(const std::string &value) override {
    if (isNumber(value)) {
      int num = std::stoi(value);
      return (num >= 2020) && (num <= 2030);
    }
    return false;
  }
};

struct hgt_validator : public validator {
  bool validate(const std::string &value) override {
    auto cmPos = value.find("cm");
    auto inPos = value.find("in");

    if ((cmPos < value.size()) && isNumber(value.substr(0, cmPos))) {
      int num = std::stoi(value.substr(0, cmPos));
      return (num >= 150) && (num <= 193);
    } else if ((inPos < value.size()) && isNumber(value.substr(0, inPos))) {
      int num = std::stoi(value.substr(0, cmPos));
      return (num >= 59) && (num <= 76);
    }
    return false;
  }
};

struct hcl_validator : public validator {
  bool validate(const std::string &value) override {
    auto pos = value.find("#");
    if ((value.size() == 7) && (pos == 0)) {
      return std::all_of(value.cbegin() + 1, value.cend(), [](auto c) {
        return std::isdigit(c) || ((c >= 'a') && (c <= 'f'));
      });
    }
    return false;
  }
};

struct ecl_validator : public validator {
  const std::vector<std::string> validValues = {"amb", "blu", "brn", "gry",
                                                "grn", "hzl", "oth"};
  bool validate(const std::string &value) override {
    return std::any_of(validValues.cbegin(), validValues.cend(),
                       [=](auto &val) { return val == value; });
  }
};

struct pid_validator : public validator {
  bool validate(const std::string &value) override {
    return (value.size() == 9) && isNumber(value);
  }
};

struct cid_validator : public validator {
  bool validate(const std::string &value) override { return true; }
};

bool part2IsValid(const std::map<std::string, std::string> &passport) {
  std::vector<std::pair<std::string, std::unique_ptr<validator>>> validationFns;
  validationFns.emplace_back("byr", std::make_unique<byr_validator>());
  validationFns.emplace_back("iyr", std::make_unique<iyr_validator>());
  validationFns.emplace_back("eyr", std::make_unique<eyr_validator>());
  validationFns.emplace_back("hcl", std::make_unique<hcl_validator>());
  validationFns.emplace_back("hgt", std::make_unique<hgt_validator>());
  validationFns.emplace_back("ecl", std::make_unique<ecl_validator>());
  validationFns.emplace_back("pid", std::make_unique<pid_validator>());
  validationFns.emplace_back("cid", std::make_unique<cid_validator>());

  bool isValid = true;
  isValid = isValid && part1IsValid(passport);
  for (const auto &keyValue : passport) {
    const auto &value = keyValue.second;
    const auto &it =
        std::find_if(validationFns.cbegin(), validationFns.cend(),
                     [=](auto &p) { return p.first == keyValue.first; });

    if (it != validationFns.cend()) {
      auto res = it->second->validate(value);
      isValid = isValid && res;
    } else {
      isValid = false;
    }
  }
  return isValid;
}

void countValidPassports(const std::vector<std::string> &lines,
                         decltype(part1IsValid) fn) {
  auto passports = parsePassports(lines);
  auto count = std::accumulate(
      passports.cbegin(), passports.cend(), 0,
      [=](auto acc, auto passport) { return fn(passport) ? acc + 1 : acc; });
  print(count);
}

int main() {
  auto lines = readLinesFromFile("input/day4.txt");
  countValidPassports(lines, part1IsValid);
  countValidPassports(lines, part2IsValid);
}
