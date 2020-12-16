#include "bits-and-bobs.hh"
#include <unordered_set>

using namespace std;

struct Notes {
  vector<string> rangeNames;
  vector<vector<pair<int, int>>> validRanges;
  vector<int> yourTicket;
  vector<vector<int>> nearbyTickets;
};

Notes parseNotes(const vector<string> &lines) {
  vector<string> rangeNames;
  vector<vector<pair<int, int>>> validRanges;
  vector<int> yourTicket;
  vector<vector<int>> nearbyTickets;

  auto it = lines.cbegin();
  while (it != lines.cend()) {
    std::regex isRangeRe("([a-z\\s]+):\\s(.*)");
    std::smatch isRangeMatch;
    std::regex_match(*it, isRangeMatch, isRangeRe);
    if (!isRangeMatch.empty()) {
      string rangeName = isRangeMatch.str(1);
      string ranges = isRangeMatch.str(2);
      vector<pair<int, int>> validRangesForPosition;
      for (;;) {
        std::regex rangeRe("(\\d+)-(\\d+)(\\sor\\s)?(.*)");
        std::smatch rangeMatch;
        std::regex_match(ranges, rangeMatch, rangeRe);
        assert(!rangeMatch.empty());
        int lower = std::stoi(rangeMatch.str(1));
        int upper = std::stoi(rangeMatch.str(2));
        ranges = rangeMatch.str(4);
        validRangesForPosition.push_back({lower, upper});
        if (ranges.empty()) {
          break;
        }
      }
      rangeNames.push_back(rangeName);
      validRanges.push_back(std::move(validRangesForPosition));
    }

    if (*it == "your ticket:") {
      it++;
      auto yourTicketStr = splitString(*it, ",");
      transform(yourTicketStr.cbegin(), yourTicketStr.cend(),
                std::back_inserter(yourTicket),
                [](auto s) { return std::stoi(s); });
    }

    if (*it == "nearby tickets:") {
      it++;
      while ((it != lines.cend()) && (!it->empty())) {
        auto ticketStr = splitString(*it, ",");
        vector<int> ticket;
        transform(ticketStr.cbegin(), ticketStr.cend(),
                  std::back_inserter(ticket),
                  [](auto s) { return std::stoi(s); });
        nearbyTickets.push_back(std::move(ticket));
        it++;
      }
    }

    if (it == lines.cend()) {
      break;
    }
    it++;
  }
  return {rangeNames, validRanges, yourTicket, nearbyTickets};
}

int calculateErrorRate(const Notes &notes, const vector<int> &ticket) {
  auto checkValue = [&](int acc, int value) {
    int errorValue = value;
    for (const auto &ranges : notes.validRanges) {
      for (const auto &range : ranges) {
        if ((range.first <= value) && (value <= range.second)) {
          errorValue = 0;
        }
      }
    }
    return acc + errorValue;
  };

  return std::accumulate(ticket.cbegin(), ticket.cend(), 0, checkValue);
}

void part1(const Notes &notes) {
  int ticketSanningErrorRate =
      std::accumulate(notes.nearbyTickets.cbegin(), notes.nearbyTickets.cend(),
                      0, [&](auto acc, const auto &ticket) {
                        assert(ticket.size() == notes.validRanges.size());
                        return acc + calculateErrorRate(notes, ticket);
                      });
  print(ticketSanningErrorRate);
}

void part2(const Notes &notes) {
  vector<vector<int>> validNearbyTickets;
  std::copy_if(notes.nearbyTickets.cbegin(), notes.nearbyTickets.cend(),
               std::back_inserter(validNearbyTickets), [&](const auto &ticket) {
                 return calculateErrorRate(notes, ticket) == 0;
               });

  vector<vector<int>> valuesByPosition;
  int ticketSize = validNearbyTickets.at(0).size();
  for (int i = 0; i < ticketSize; i++) {
    valuesByPosition.push_back(std::move(getColumn(i, validNearbyTickets)));
  }

  // for each field, record which position it could be, then assign positions
  vector<vector<int>> possiblePositionsForRange(notes.validRanges.size());
  for (size_t rangeIdx = 0; rangeIdx < notes.validRanges.size(); rangeIdx++) {
    const auto &ranges = notes.validRanges.at(rangeIdx);

    auto valueMatchesRanges = [&ranges](int value) {
      bool isMatch = std::any_of(ranges.cbegin(), ranges.cend(),
                                 [&value](const pair<int, int> &range) {
                                   return (range.first <= value) &&
                                          (value <= range.second);
                                 });
      return isMatch;
    };
    // for each position, if this ranges matches all values, push
    for (size_t posIdx = 0; posIdx < valuesByPosition.size(); posIdx++) {
      const auto &values = valuesByPosition.at(posIdx);
      bool allValuesMatch =
          std::all_of(values.cbegin(), values.cend(), valueMatchesRanges);
      if (allValuesMatch) {
        possiblePositionsForRange.at(rangeIdx).push_back(posIdx);
      }
    }
  }

  // assign positions
  unordered_set<int> assignedPositions;
  for (;;) {
    // find a position that we know definitely but that
    // is in a possibility for others
    const auto posForRangePtr = std::find_if(
        possiblePositionsForRange.cbegin(), possiblePositionsForRange.cend(),
        [&assignedPositions](const auto &idxs) {
          return (idxs.size() == 1) && (assignedPositions.find(idxs.at(0)) ==
                                        assignedPositions.cend());
        });
    if (posForRangePtr == possiblePositionsForRange.cend()) {
      break;
    }

    // remove position
    int pos = posForRangePtr->at(0);
    assignedPositions.insert(pos);
    for (auto &posns : possiblePositionsForRange) {
      if (posns.size() > 1) {
        auto e = std::find(posns.cbegin(), posns.cend(), pos);
        if (e != posns.cend()) {
          posns.erase(e);
        }
      }
    }
  }

  long product = 1;
  for (size_t rangeIdx = 0; rangeIdx < notes.validRanges.size(); rangeIdx++) {
    assert(possiblePositionsForRange.at(rangeIdx).size() == 1);
    const auto &rangeName = notes.rangeNames.at(rangeIdx);
    if (startsWith(rangeName, "departure")) {
      int posIdx = possiblePositionsForRange.at(rangeIdx).at(0);
      product *= notes.yourTicket.at(posIdx);
    }
  }
  print(product);
}

int main() {
  auto lines = readLinesFromFile("input/day16.txt");
  auto notes = parseNotes(lines);
  part1(notes);
  part2(notes);
}
