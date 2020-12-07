#include "bits-and-bobs.hh"
#include <unordered_set>

using namespace std;

struct BagWithSubbags {
  string name;
  vector<pair<string, int>> subBags;
};

BagWithSubbags parseBagsWithSubbags(const std::string &line) {
  vector<pair<string, int>> subBags;
  std::regex r("([a-zA-Z\\s]+)\\sbags\\scontain\\s(.*)");
  std::smatch match;
  std::regex_match(line, match, r);
  std::string bag = match.str(1);
  std::string rest = match.str(2);
  if (rest != "no other bags.") {
    for (;;) {
      std::regex r2("(\\d+)\\s([a-zA-Z\\s]+)\\sbags?,?\\.?\\s?(.*)");
      std::smatch match2;
      std::regex_match(rest, match2, r2);
      string count{match2.str(1)};
      string subBag{match2.str(2)};
      rest = match2.str(3);
      subBags.push_back({subBag, std::stoi(count)});
      if (rest.empty()) {
        break;
      }
    }
  }
  return {bag, subBags};
}

void part1(const vector<BagWithSubbags> &bagsWithSubbags) {
  string elemToCount{"shiny gold"};
  unordered_map<string, unordered_set<string>> containedInGraph;
  for (const auto &bagWithSubbags : bagsWithSubbags) {
    for (const auto &p : bagWithSubbags.subBags) {
      if (containedInGraph.find(p.first) == containedInGraph.end()) {
        containedInGraph.insert({p.first, {}});
      }
      containedInGraph.at(p.first).insert(bagWithSubbags.name);
    }
  }

  deque<string> nodes;
  unordered_set<string> seenBags;
  for (const auto &n : containedInGraph.at(elemToCount)) {
    nodes.push_back(n);
  }
  while (!nodes.empty()) {
    string currentNode{nodes.front()};
    nodes.pop_front();
    seenBags.insert(currentNode);
    if (containedInGraph.find(currentNode) != containedInGraph.end()) {
      const auto &subNodes = containedInGraph.at(currentNode);
      for (const auto &n : subNodes) {
        nodes.push_back(n);
      }
    }
  }
  print(seenBags.size());
}

void part2(const vector<BagWithSubbags> &bagsWithSubbags) {
  string elemToCount{"shiny gold"};

  deque<string> callStack;
  unordered_map<string, int> cache;
  callStack.push_back(elemToCount);

  while (!callStack.empty()) {
    const auto &currentNode = callStack.back();
    auto bagWithSubbagPtr =
        std::find_if(bagsWithSubbags.cbegin(), bagsWithSubbags.cend(),
                     [&](const auto &b) { return b.name == currentNode; });
    assert(bagWithSubbagPtr != bagsWithSubbags.cend());
    const BagWithSubbags &bagWithSubbag = *bagWithSubbagPtr;

    // If we have them all, compute, cache, pop_back
    // otherwise queue what we don't have
    bool allComputed = true;
    int count = 0;
    for (const auto &subBag : bagWithSubbag.subBags) {
      if (cache.find(subBag.first) != cache.end()) {
        count += subBag.second * (1 + cache.at(subBag.first));
      } else {
        allComputed = false;
        callStack.push_back(subBag.first);
      }
    }
    if (allComputed) {
      cache.insert({currentNode, count});
      callStack.pop_back();
    }
  }
  print(cache.at(elemToCount));
}

int main() {
  auto lines = readLinesFromFile("input/day7.txt");
  vector<BagWithSubbags> bagsWithSubbags(lines.size());
  transform(lines.cbegin(), lines.cend(), bagsWithSubbags.begin(),
            parseBagsWithSubbags);
  part1(bagsWithSubbags);
  part2(bagsWithSubbags);
}
