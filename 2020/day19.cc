#include "bits-and-bobs.hh"

using namespace std;

// enum class RuleType { CharRule, SumRule, OrRule };

class Rule {
public:
  Rule(int i) : ruleNum{i} {};
  virtual ~Rule(){};
  virtual void print() const = 0;
  virtual bool check(string::const_iterator &it) const = 0;

  int ruleNum;
};

class CharRule : public Rule {
public:
  CharRule(int i, char c_) : Rule{i}, c{c_} {}
  char c;

  void print() const override { cout << ruleNum << ": " << c << "\n"; }
  bool check(string::const_iterator &it) const { return *it++ == c; }
};

class AndRule : public Rule {
public:
  AndRule(int i, vector<shared_ptr<Rule>> &&subRules)
      : Rule{i}, rules{std::move(subRules)} {};

  vector<shared_ptr<Rule>> rules;

  void print() const override {
    cout << ruleNum << ": ";
    for (const auto &p : rules) {
      cout << p->ruleNum << " ";
    }
    cout << "\n";
  }
  bool check(string::const_iterator &it) const {
    return all_of(rules.cbegin(), rules.cend(),
                  [&it](const auto &rule) { return rule->check(it); });
  }
};

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

  bool check(string::const_iterator &it) const {
    string::const_iterator startIt = it;
    for (const auto &rules : ruleGroups) {
      bool isMatch =
          all_of(rules.cbegin(), rules.cend(),
                 [&it](const auto &rule) { return rule->check(it); });
      if (isMatch) {
        return true;
      }
      it = startIt;
    }
    return false;
  }
};

unordered_map<int, shared_ptr<Rule>>
parseRuleDefinitions(const unordered_map<int, string> &ruleDefinitions) {
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
                {ruleNum, make_shared<AndRule>(ruleNum, std::move(subRules))});
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
  return rules;
}

void part1(const vector<string> &lines) {
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

  auto rules = parseRuleDefinitions(ruleDefinitions);

  auto it = lines.cbegin();
  while (!it++->empty()) {
  }
  for (; it != lines.cend(); it++) {
    print(*it);
    string::const_iterator strIt = it->cbegin();
    print(rules.at(0)->check(strIt));
  }
}

int main() {
  auto lines = readLinesFromFile("input/day19.txt");
  part1(lines);
  // string s = "ab";
  // string::const_iterator strIt = s.cbegin();
  // // CharRule r{0, 'a'};
  // auto ra = make_shared<CharRule>(1, 'a');
  // auto rb = make_shared<CharRule>(2, 'b');
  // OrRule r{0, {{ra, rb}, {rb, ra}}};
  // print(r.check(strIt));
}
