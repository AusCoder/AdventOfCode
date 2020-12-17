#include "bits-and-bobs.hh"
#include <unordered_set>

using namespace std;

struct Point {
  int x;
  int y;
  int z;
};

bool operator==(const Point &p1, const Point &p2) {
  return (p1.x == p2.x) && (p1.y == p2.y) && (p1.z == p2.z);
}

namespace std {
template <> struct hash<Point> {
  std::size_t operator()(const Point &p) const noexcept {
    std::size_t h = hash<int>()(p.x) ^ (hash<int>()(p.y) << 1);
    return (h >> 1) ^ (hash<int>()(p.z));
  }
};
} // namespace std

vector<Point> neighbors(const Point &p) {
  vector<Point> ps;
  for (int x = 0; x < 2; x++) {
    for (int y = 0; y < 2; y++) {
      for (int z = 0; z < 2; z++) {
        ps.push_back({p.x + 2 * x - 1, p.y + 2 * y - 1, p.z + 2 * z - 1});
      }
    }
  }
  return ps;
}

struct Universe {
public:
  Universe(const vector<vector<char>> &init)
      : universe{}, xMin{0}, xMax{static_cast<int>(init.at(0).size())}, yMin{0},
        yMax{static_cast<int>(init.size())}, zMin{0}, zMax{1} {
    for (size_t y = 0; y < init.size(); y++) {
      for (size_t x = 0; x < init.at(y).size(); x++) {
        if (init.at(y).at(x) == '#') {
          universe.insert({static_cast<int>(x), static_cast<int>(y), 0});
        }
      }
    }
  }

  void printLayer(int z) const {
    vector<vector<char>> plane;
    for (int y = yMin; y < yMax; y++) {
      vector<char> row(xMax - xMin);
      std::fill(row.begin(), row.end(), '.');
      plane.push_back(std::move(row));
    }
    for (const auto &p : universe) {
      if (p.z == z) {
        plane.at(p.y).at(p.x) = '#';
      }
    }
    printMatrix(plane);
  }

  // void runCycle() {
  //   // 2 things: nodes to create, nodes to destroy
  //   // creating nodes mean iterating all the neighbors of the current points
  //   // destroying current nodes means iterating current nodes

  //   unordered_set<Point> newUniverse;
  //   for (const auto &p : universe) {

  //   }
  // }

private:
  unordered_set<Point> universe;
  int xMin, xMax, yMin, yMax, zMin, zMax;
};

int main() {
  auto lines = readLinesFromFile("input/day17.txt");
  auto initUniverse = parseMatrixChar(lines);
  Universe universe{initUniverse};
  universe.printLayer(0);

  Point p{0, 0, 0};
  print(neighbors(p).size());
}
