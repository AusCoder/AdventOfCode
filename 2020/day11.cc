#include "bits-and-bobs.hh"

using namespace std;

bool isSame(const vector<vector<char>> &previousLayout,
            const vector<vector<char>> &layout) {
  bool ret = true;
  for (size_t j = 0; j < previousLayout.size(); j++) {
    for (size_t i = 0; i < previousLayout.at(0).size(); i++) {
      ret &= (previousLayout.at(j).at(i) == layout.at(j).at(i));
    }
  }
  return ret;
}

class adj_strategy {
public:
  virtual bool isAllAdjUnoccupied(const vector<vector<char>> &layout, int x,
                                  int y) const = 0;

  virtual bool isAllAdjOccupied(const vector<vector<char>> &layout, int x,
                                int y) const = 0;
};

class part1_adj_strategy : public adj_strategy {
public:
  bool isAllAdjUnoccupied(const vector<vector<char>> &layout, int x,
                          int y) const {
    int height = layout.size();
    int width = layout.at(0).size();
    auto adjPos = adjacentPositions2D(x, y, width, height);
    bool allAdjUnoccupied =
        all_of(adjPos.cbegin(), adjPos.cend(), [&](const auto &pos) {
          return layout.at(pos.second).at(pos.first) != '#';
        });
    return allAdjUnoccupied;
  }

  bool isAllAdjOccupied(const vector<vector<char>> &layout, int x,
                        int y) const {
    int height = layout.size();
    int width = layout.at(0).size();
    auto adjPos = adjacentPositions2D(x, y, width, height);
    int occupiedCount = accumulate(
        adjPos.cbegin(), adjPos.cend(), 0, [&](auto acc, const auto &pos) {
          return layout.at(pos.second).at(pos.first) == '#' ? acc + 1 : acc;
        });
    return occupiedCount >= 4;
  }
};

class part2_adj_strategy : public adj_strategy {
public:
  bool isAllAdjUnoccupied(const vector<vector<char>> &layout, int x,
                          int y) const {
    auto ns = neighbors(layout, x, y);
    bool allAdjUnoccupied =
        all_of(ns.cbegin(), ns.cend(), [&](const auto &n) { return n != '#'; });
    return allAdjUnoccupied;
  }

  bool isAllAdjOccupied(const vector<vector<char>> &layout, int x,
                        int y) const {
    auto ns = neighbors(layout, x, y);
    int occupiedCount =
        accumulate(ns.cbegin(), ns.cend(), 0, [&](auto acc, const auto &n) {
          return n == '#' ? acc + 1 : acc;
        });
    return occupiedCount >= 5;
  }

private:
  vector<char> neighbors(const vector<vector<char>> &layout, int startx,
                         int starty) const {
    int height = layout.size();
    int width = layout.at(0).size();
    vector<char> ns;
    char c = '.';
    // x direction
    for (int x = startx + 1; x < width; x++) {
      c = layout.at(starty).at(x);
      if (c != '.') {
        break;
      }
    }
    ns.push_back(c);

    c = '.';
    for (int x = startx - 1; x >= 0; x--) {
      c = layout.at(starty).at(x);
      if (c != '.') {
        break;
      }
    }
    ns.push_back(c);
    // y direction
    c = '.';
    for (int y = starty + 1; y < height; y++) {
      c = layout.at(y).at(startx);
      if (c != '.') {
        break;
      }
    }
    ns.push_back(c);

    c = '.';
    for (int y = starty - 1; y >= 0; y--) {
      c = layout.at(y).at(startx);
      if (c != '.') {
        break;
      }
    }
    ns.push_back(c);
    // diagonals
    c = '.';
    for (int x = startx + 1, y = starty + 1; x < width && y < height;
         x++, y++) {
      c = layout.at(y).at(x);
      if (c != '.') {
        break;
      }
    }
    ns.push_back(c);

    c = '.';
    for (int x = startx + 1, y = starty - 1; x < width && y >= 0; x++, y--) {
      c = layout.at(y).at(x);
      if (c != '.') {
        break;
      }
    }
    ns.push_back(c);

    c = '.';
    for (int x = startx - 1, y = starty + 1; x >= 0 && y < height; x--, y++) {
      c = layout.at(y).at(x);
      if (c != '.') {
        break;
      }
    }
    ns.push_back(c);

    c = '.';
    for (int x = startx - 1, y = starty - 1; x >= 0 && y >= 0; x--, y--) {
      c = layout.at(y).at(x);
      if (c != '.') {
        break;
      }
    }
    ns.push_back(c);
    return ns;
  }
};

void run(const adj_strategy &strat, const vector<vector<char>> &l) {
  vector<vector<char>> layout{l.begin(), l.end()};
  int height = layout.size();
  int width = layout.at(0).size();

  for (;;) {
    vector<vector<char>> nextLayout{l.begin(), l.end()};
    // evolve layout
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        auto current = layout.at(y).at(x);
        char next = '_';
        if (current == 'L') {
          bool allAdjUnoccupied = strat.isAllAdjUnoccupied(layout, x, y);
          if (allAdjUnoccupied) {
            next = '#';
          }
        } else if (current == '#') {
          bool allAdjOccupied = strat.isAllAdjOccupied(layout, x, y);
          if (allAdjOccupied) {
            next = 'L';
          }
        }
        if (next == '_') {
          next = current;
        }
        nextLayout.at(y).at(x) = next;
      }
    }

    // check for stable layout
    if (isSame(layout, nextLayout)) {
      break;
    }
    layout = nextLayout;
  }

  auto count = accumulate(
      layout.cbegin(), layout.cend(), 0, [](auto acc, const auto &row) {
        return acc +
               accumulate(row.cbegin(), row.cend(), 0,
                          [](auto a, auto c) { return c == '#' ? a + 1 : a; });
      });
  print(count);
}

int main() {
  auto lines = readLinesFromFile("input/day11.txt");
  auto matrix = parseMatrixChar(lines);
  part1_adj_strategy part1_strat;
  part2_adj_strategy part2_strat;
  run(part1_strat, matrix);
  run(part2_strat, matrix);
}
