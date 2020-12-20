#include "bits-and-bobs.hh"
#include <unordered_set>

using namespace std;

class Rule {
public:
  Rule(int i) : ruleNum{i} {};
  virtual ~Rule(){};
  virtual void print() const = 0;
  virtual void replace(shared_ptr<Rule> rule) = 0;
  bool check(string::const_iterator &it, string::const_iterator end) const {
    return doCheck(it, end) && checkLast(it, end);
  }
  virtual vector<string> build() const = 0;
  int ruleNum;

private:
  virtual bool doCheck(string::const_iterator &it,
                       string::const_iterator end) const = 0;

  bool checkLast(string::const_iterator &it, string::const_iterator end) const {
    if (ruleNum == 0) {
      return it == end;
    }
    return true;
  }
};

class CharRule : public Rule {
public:
  CharRule(int i, char c_) : Rule{i}, c{c_} {}

  void print() const override { cout << ruleNum << ": " << c << "\n"; }

  void replace(shared_ptr<Rule> rule) {}

  vector<string> build() const override {
    string s{c};
    return {s};
  }

private:
  char c;
  bool doCheck(string::const_iterator &it,
               string::const_iterator end) const override {
    return *it++ == c;
  }
};

vector<string> buildProduct(const vector<shared_ptr<Rule>> &rules) {
  vector<string> possibilities;
  for (const auto &rule : rules) {
    auto rulePossibilities = rule->build();
    if (possibilities.size() == 0) {
      possibilities = rulePossibilities;
    } else {
      vector<string> newPossibilities;
      for (auto &p : possibilities) {
        for (const auto &rp : rulePossibilities) {
          newPossibilities.push_back(p + rp);
        }
      }
      possibilities = std::move(newPossibilities);
    }
  }
  return possibilities;
}

class OrRule : public Rule {
public:
  OrRule(int i, vector<vector<shared_ptr<Rule>>> &&subRules)
      : Rule{i}, ruleGroups{std::move(subRules)} {};

  vector<vector<shared_ptr<Rule>>> ruleGroups;

  void print() const override {
    cout << ruleNum << ": ";
    for (const auto &rules : ruleGroups) {
      for (const auto &p : rules) {
        cout << p->ruleNum << " ";
      }
      cout << "| ";
    }
    cout << "\n";
  }

  void replace(shared_ptr<Rule> rule) {
    for (auto &rules : ruleGroups) {
      for (auto it = rules.begin(); it != rules.end(); it++) {
        if ((*it)->ruleNum == rule->ruleNum) {
          *it = rule;
        }
      }
    }
  }

  vector<string> build() const override {
    vector<string> possibilities;
    for (const auto &rules : ruleGroups) {
      auto rulePossibilities = buildProduct(rules);
      for (const auto &p : rulePossibilities) {
        possibilities.push_back(p);
      }
    }
    return possibilities;
  }

private:
  bool doCheck(string::const_iterator &it,
               string::const_iterator end) const override {

    string::const_iterator startIt = it;
    for (const auto &rules : ruleGroups) {
      bool isMatch =
          all_of(rules.cbegin(), rules.cend(), [&it, &end](const auto &rule) {
            return rule->check(it, end);
          });
      if (isMatch) {
        return true;
      }
      it = startIt;
    }
    return false;
  }
};

class AndRule : public Rule {
public:
  AndRule(int i, bool hasModifiedRules_, vector<shared_ptr<Rule>> &&subRules)
      : Rule{i}, hasModifiedRules{hasModifiedRules_}, rules{std::move(
                                                          subRules)} {};

  void print() const override {
    cout << ruleNum << ": ";
    for (const auto &p : rules) {
      cout << p->ruleNum << " ";
    }
    cout << "\n";
  }

  void replace(shared_ptr<Rule> rule) {
    for (auto it = rules.begin(); it != rules.end(); it++) {
      if ((*it)->ruleNum == rule->ruleNum) {
        *it = rule;
      }
    }
  }
  bool hasModifiedRules;
  vector<shared_ptr<Rule>> rules;

  vector<string> build() const override { return buildProduct(rules); }

private:
  bool doCheck(string::const_iterator &it,
               string::const_iterator end) const override {

    if (hasModifiedRules && (ruleNum == 0)) {
      // This is rough
      // Manually checking the rules:
      // 0: 8 11
      // 8: 42 | 42 8
      // 11: 42 31 | 42 11 31
      //
      // General idea is: eat some 42s then try eating 31s

      auto rule8 = rules.at(0);
      assert(rule8->ruleNum == 8);
      auto rule11 = rules.at(1);
      assert(rule11->ruleNum == 11);
      OrRule *rule8Cast = dynamic_cast<OrRule *>(rule8.get());
      auto rule42 = rule8Cast->ruleGroups.at(0).at(0);
      assert(rule42->ruleNum == 42);
      OrRule *rule11Cast = dynamic_cast<OrRule *>(rule11.get());
      auto rule31 = rule11Cast->ruleGroups.at(0).at(1);
      assert(rule31->ruleNum == 31);

      string::const_iterator startIt = it;
      for (;;) {
        // parse one 42
        if (!rule42->check(it, end)) {
          it = startIt;
          return false;
        }
        string::const_iterator parsed42It = it;
        // Try parsing some 42's then same number of 31's
        string::const_iterator checkpointIt = it;
        int parseCount = 0;
        while (rule42->check(it, end)) {
          checkpointIt = it;
          parseCount++;
        }
        it = checkpointIt;
        if (parseCount == 0) {
          it = startIt;
          return false;
        }
        bool isMatch = true;
        for (int i = 0; i < parseCount; i++) {
          if (!rule31->check(it, end)) {
            it = parsed42It;
            isMatch = false;
            break;
          }
        }
        if (isMatch) {
          return true;
        }
      }
      assert(false);
      return false;
    }

    return all_of(rules.cbegin(), rules.cend(), [&it, &end](const auto &rule) {
      return rule->check(it, end);
    });
  }
};

unordered_map<int, shared_ptr<Rule>>
parseRuleDefinitions(bool hasModifiedRules, const vector<string> &lines) {
  unordered_map<int, string> ruleDefinitions;
  for (const auto &line : lines) {
    if (line.empty()) {
      break;
    }
    std::regex ruleRe("(\\d+):\\s(.*)");
    std::smatch ruleMatch;
    std::regex_match(line, ruleMatch, ruleRe);
    assert(!ruleMatch.empty());
    int ruleNum = std::stoi(ruleMatch.str(1));
    string ruleDefinition = ruleMatch.str(2);
    ruleDefinitions.insert({ruleNum, ruleDefinition});
  }

  unordered_map<int, shared_ptr<Rule>> rules;

  while (rules.size() < ruleDefinitions.size()) {
    for (const auto &pair : ruleDefinitions) {
      int ruleNum = pair.first;
      string definition = pair.second;
      if (rules.find(ruleNum) == rules.cend()) {
        // Char rule
        std::regex charRuleRe("\"([a-z])\"");
        std::smatch charRuleMatch;
        std::regex_match(definition, charRuleMatch, charRuleRe);
        if (!charRuleMatch.empty()) {
          string c = charRuleMatch.str(1);
          assert(c.size() == 1);
          rules.insert({ruleNum, make_shared<CharRule>(ruleNum, c.at(0))});
          continue;
        }
        // And rule
        std::regex andRuleRe("[0-9\\s]+");
        std::smatch andRuleMatch;
        std::regex_match(definition, andRuleMatch, andRuleRe);
        if (!andRuleMatch.empty()) {
          auto ruleNums = parseVectorInt(definition);
          bool hasAllSubrules =
              all_of(ruleNums.cbegin(), ruleNums.cend(), [&rules](auto n) {
                return rules.find(n) != rules.cend();
              });
          if (hasAllSubrules) {
            vector<shared_ptr<Rule>> subRules;
            transform(ruleNums.cbegin(), ruleNums.cend(),
                      back_inserter(subRules),
                      [&rules](auto n) { return rules.at(n); });
            rules.insert(
                {ruleNum, make_shared<AndRule>(ruleNum, hasModifiedRules,
                                               std::move(subRules))});
          }
          continue;
        }
        // Or rule
        vector<vector<int>> ruleGroupNums;
        while (!definition.empty()) {
          std::regex orRuleGroupRe("([0-9\\s]+)\\|?(.*)");
          std::smatch orRuleGroupMatch;
          std::regex_match(definition, orRuleGroupMatch, orRuleGroupRe);
          assert(!orRuleGroupMatch.empty());
          string ruleGroup = orRuleGroupMatch.str(1);
          ruleGroupNums.push_back(std::move(parseVectorInt(ruleGroup)));
          definition = orRuleGroupMatch.str(2);
        }
        bool hasAllSubrules =
            all_of(ruleGroupNums.cbegin(), ruleGroupNums.cend(),
                   [&rules](const auto &group) {
                     return all_of(group.cbegin(), group.cend(),
                                   [&rules](const auto &n) {
                                     return rules.find(n) != rules.cend();
                                   });
                   });
        if (hasAllSubrules) {
          vector<vector<shared_ptr<Rule>>> ruleGroups;
          for (const auto &ruleNums : ruleGroupNums) {
            vector<shared_ptr<Rule>> subRules;
            transform(ruleNums.cbegin(), ruleNums.cend(),
                      back_inserter(subRules),
                      [&rules](int n) { return rules.at(n); });
            ruleGroups.push_back(std::move(subRules));
          }
          rules.insert(
              {ruleNum, make_shared<OrRule>(ruleNum, std::move(ruleGroups))});
        }
      }
    }
  }

  if (hasModifiedRules) {
    vector<vector<shared_ptr<Rule>>> ruleGroups8{{rules.at(42)},
                                                 {rules.at(42)}};
    auto new8 = make_shared<OrRule>(8, std::move(ruleGroups8));
    new8->ruleGroups.at(1).push_back(new8);

    vector<vector<shared_ptr<Rule>>> ruleGroups11{{rules.at(42), rules.at(31)},
                                                  {rules.at(42), rules.at(31)}};
    auto new11 = make_shared<OrRule>(11, std::move(ruleGroups11));
    new11->ruleGroups.at(1).insert(new11->ruleGroups.at(1).begin() + 1, new11);

    rules.at(8) = new8;
    rules.at(11) = new11;
    for (auto &p : rules) {
      if (p.first != 8) {
        p.second->replace(new8);
      }
      if (p.first != 11) {
        p.second->replace(new11);
      }
    }
  }
  return rules;
}

void run(const vector<string> &lines,
         const unordered_map<int, shared_ptr<Rule>> &rules) {
  auto it = lines.cbegin();
  while (!it++->empty()) {
  }
  int validCount = 0;
  for (; it != lines.cend(); it++) {
    string::const_iterator strIt = it->cbegin();
    string::const_iterator endIt = it->cend();
    if (rules.at(0)->check(strIt, endIt)) {
      validCount++;
    }
  }
  print(validCount);
}

int main() {
  auto lines = readLinesFromFile("input/day19.txt");
  auto part1Rules = parseRuleDefinitions(false, lines);
  auto part2Rules = parseRuleDefinitions(true, lines);
  run(lines, part1Rules);
  run(lines, part2Rules);
}
