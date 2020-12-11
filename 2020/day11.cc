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

void part1(const vector<vector<char>> &l) {
  vector<vector<char>> layout{l.begin(), l.end()};
  int height = layout.size();
  int width = layout.at(0).size();
  // printMatrix(layout);
  // print("");
  for (;;) {
    vector<vector<char>> nextLayout{l.begin(), l.end()};
    // evolve layout
    for (int j = 0; j < height; j++) {
      for (int i = 0; i < width; i++) {
        auto current = layout.at(j).at(i);
        char next = '_';
        if (current == 'L') {
          auto adjPos = adjacentPositions2D(i, j, width, height);
          bool allAdjUnoccupied =
              all_of(adjPos.cbegin(), adjPos.cend(), [&](const auto &pos) {
                return layout.at(pos.second).at(pos.first) != '#';
              });
          if (allAdjUnoccupied) {
            next = '#';
          }
        } else if (current == '#') {
          auto adjPos = adjacentPositions2D(i, j, width, height);
          int occupiedCount =
              accumulate(adjPos.cbegin(), adjPos.cend(), 0,
                         [&](auto acc, const auto &pos) {
                           return layout.at(pos.second).at(pos.first) == '#'
                                      ? acc + 1
                                      : acc;
                         });
          if (occupiedCount >= 4) {
            next = 'L';
          }
        }
        if (next == '_') {
          next = current;
        }
        nextLayout.at(j).at(i) = next;
      }
    }

    if (isSame(layout, nextLayout)) {
      // printMatrix(layout);
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
  part1(matrix);
  // adjacentPositions(0, 1, 3, 3);
}
