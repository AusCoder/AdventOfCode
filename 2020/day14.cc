#include "bits-and-bobs.hh"

using namespace std;

string maskToBin(long mask) {
  string s(36, '0');
  for (int i = 0; i < 36; i++) {
    if ((1L << i) & mask) {
      s[35 - i] = '1';
    }
  }
  return s;
}

string parseMaskStr(const string &line) {
  std::regex maskRe("mask\\s=\\s([X01]+)");
  std::smatch maskMatch;
  std::regex_match(line, maskMatch, maskRe);
  assert(!maskMatch.empty());
  return maskMatch.str(1);
}

pair<long, long> parseMaskToAndOrMasks(const string &line) {
  string maskStr = parseMaskStr(line);
  // setting 1's is an Or mask: 0 -> 1, 1 -> 1
  // setting 0's is an And mask: 0 -> 0, 1 -> 0
  unsigned long andMask = 0;
  unsigned long orMask = 0;
  for (size_t i = 0; i < maskStr.size(); i++) {
    auto c = maskStr.at(i);
    auto shiftSize = maskStr.size() - 1 - i;
    if (c == 'X') {
      andMask |= (1L << shiftSize);
    } else if (c == '0') {
    } else if (c == '1') {
      orMask |= (1L << shiftSize);
      andMask |= (1L << shiftSize);
    } else {
      throw std::runtime_error("unknown char");
    }
  }
  return {andMask, orMask};
}

vector<string>::const_iterator
parseAddrWithValue(vector<string>::const_iterator beg,
                   vector<string>::const_iterator end,
                   vector<pair<long, long>> &addrWithValue) {
  std::regex memRe("mem\\[(\\d+)\\]\\s=\\s(\\d+)");
  for (; beg != end; beg++) {
    std::smatch memMatch;
    std::regex_match(*beg, memMatch, memRe);
    if (memMatch.empty()) {
      break;
    }
    long addr{std::stol(memMatch.str(1))};
    long value{std::stol(memMatch.str(2))};
    addrWithValue.push_back({addr, value});
  }

  return beg;
}

void part1(const vector<string> &lines) {
  auto lineIt = lines.cbegin();
  unordered_map<long, long> memory;

  while (lineIt != lines.cend()) {
    auto mask = parseMaskToAndOrMasks(*lineIt);
    lineIt++;

    vector<pair<long, long>> addrWithValue;
    lineIt = parseAddrWithValue(lineIt, lines.cend(), addrWithValue);

    for (const auto &[addr, value] : addrWithValue) {
      if (memory.find(addr) == memory.cend()) {
        memory.insert({addr, 0});
      }
      memory.at(addr) = (value & mask.first) | mask.second;
    }
  }
  auto sum = accumulate(memory.cbegin(), memory.cend(), 0L,
                        [](auto acc, auto p) { return acc + p.second; });
  print(sum);
}

vector<long> calculateMaskedAddrs(const string &mask, long addr) {
  for (size_t i = 0; i < mask.size(); i++) {
    auto c = mask.at(i);
    int shiftSize = mask.size() - 1 - i;
    if (c == '1') {
      // set a bit to 1
      addr |= (1L << shiftSize);
    }
  }

  vector<long> addrs;
  for (size_t i = 0; i < mask.size(); i++) {
    auto c = mask.at(i);
    int shiftSize = mask.size() - 1 - i;
    if (c == 'X') {
      if (addrs.empty()) {
        addrs.push_back(addr & ~(1L << shiftSize));
        addrs.push_back(addr | (1L << shiftSize));
      } else {
        auto curAddrsSize = addrs.size();
        for (size_t addrsIdx = 0; addrsIdx < curAddrsSize; addrsIdx++) {
          long curAddr = addrs.at(addrsIdx);
          addrs.at(addrsIdx) = curAddr & ~(1L << shiftSize);
          addrs.push_back(curAddr | (1L << shiftSize));
        }
      }
    }
  }
  return addrs;
}

void part2(const vector<string> &lines) {
  auto lineIt = lines.cbegin();
  unordered_map<long, long> memory;

  while (lineIt != lines.cend()) {
    auto mask = parseMaskStr(*lineIt);
    lineIt++;
    vector<pair<long, long>> addrWithValue;
    lineIt = parseAddrWithValue(lineIt, lines.cend(), addrWithValue);

    for (const auto &[addr, value] : addrWithValue) {
      auto maskedAddrs = calculateMaskedAddrs(mask, addr);
      for (const auto &maskedAddr : maskedAddrs) {
        if (memory.find(maskedAddr) == memory.cend()) {
          memory.insert({maskedAddr, 0});
        }
        memory.at(maskedAddr) = value;
      }
    }
  }
  auto sum = accumulate(memory.cbegin(), memory.cend(), 0L,
                        [](auto acc, auto p) { return acc + p.second; });
  print(sum);
}

int main() {
  auto lines = readLinesFromFile("input/day14.txt");
  part1(lines);
  part2(lines);
}
