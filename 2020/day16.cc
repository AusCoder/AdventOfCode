#include "bits-and-bobs.hh"

using namespace std;

struct Notes {
  vector<vector<pair<int, int>>> validRanges;
  vector<int> yourTicket;
  vector<vector<int>> nearbyTickets;
};

Notes parseNotes(const vector<string> &lines) {
  vector<vector<pair<int, int>>> validRanges;
  vector<int> yourTicket;
  vector<vector<int>> nearbyTickets;

  auto it = lines.cbegin();
  while (it != lines.cend()) {
    std::regex isRangeRe("[a-z]+:\\s(.*)");
    std::smatch isRangeMatch;
    std::regex_match(*it, isRangeMatch, isRangeRe);
    if (!isRangeMatch.empty()) {
      string ranges = isRangeMatch.str(1);
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
      validRanges.push_back(validRangesForPosition);
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
        nearbyTickets.push_back(ticket);
        it++;
      }
    }

    if (it == lines.cend()) {
      break;
    }
    it++;
  }
  return {validRanges, yourTicket, nearbyTickets};
}

void part1(const Notes &notes) {
  int ticketSanningErrorRate = 0;

  auto checkTicket = [](const int &value,
                        const vector<pair<int, int>> &ranges) {
    int errorValue = value;
    for (const auto &range : ranges) {
      if ((range.first <= value) && (value <= range.second)) {
        errorValue = 0;
      }
    }
    return errorValue;
  };

  for (const auto &ticket : notes.nearbyTickets) {
    assert(ticket.size() == notes.validRanges.size());
    ticketSanningErrorRate += std::inner_product(ticket.cbegin(), ticket.cend(),
                                                 notes.validRanges.cbegin(), 0,
                                                 std::plus<int>(), checkTicket);
  }
  print(ticketSanningErrorRate);
}

int main() {
  auto lines = readLinesFromFile("input/day16.txt");
  auto notes = parseNotes(lines);
  part1(notes);
}
