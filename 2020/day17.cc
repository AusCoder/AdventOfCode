#include "bits-and-bobs.hh"
#include <unordered_set>

using namespace std;

struct Point3D {
  Point3D(int x_, int y_) : x{x_}, y{y_}, z{0} {};

  Point3D(int x_, int y_, int z_) : x{x_}, y{y_}, z{z_} {};

  int x;
  int y;
  int z;
};

bool operator==(const Point3D &p1, const Point3D &p2) {
  return (p1.x == p2.x) && (p1.y == p2.y) && (p1.z == p2.z);
}

namespace std {
template <> struct hash<Point3D> {
  std::size_t operator()(const Point3D &p) const noexcept {
    std::size_t h = hash<int>()(p.x) ^ (hash<int>()(p.y) << 1);
    return (h >> 1) ^ hash<int>()(p.z);
  }
};
} // namespace std

vector<Point3D> neighbors(const Point3D &p) {
  vector<Point3D> ps;
  for (int x = -1; x < 2; x++) {
    for (int y = -1; y < 2; y++) {
      for (int z = -1; z < 2; z++) {
        if (!((x == 0) && (y == 0) && (z == 0))) {
          ps.push_back({p.x + x, p.y + y, p.z + z});
        }
      }
    }
  }
  return ps;
}

struct Point4D {
  Point4D(int x_, int y_) : x{x_}, y{y_}, z{0}, w{0} {};

  Point4D(int x_, int y_, int z_, int w_) : x{x_}, y{y_}, z{z_}, w{w_} {};

  int x;
  int y;
  int z;
  int w;
};

bool operator==(const Point4D &p1, const Point4D &p2) {
  return (p1.x == p2.x) && (p1.y == p2.y) && (p1.z == p2.z) && (p1.w == p2.w);
}

namespace std {
template <> struct hash<Point4D> {
  std::size_t operator()(const Point4D &p) const noexcept {
    // What is the right way to combine hashes?
    std::size_t h1 = hash<int>()(p.x) ^ (hash<int>()(p.y) << 1);
    std::size_t h2 = hash<int>()(p.z) ^ (hash<int>()(p.w) << 1);
    return (h1 >> 1) ^ (h2);
  }
};
} // namespace std

vector<Point4D> neighbors(const Point4D &p) {
  vector<Point4D> ps;
  for (int x = -1; x < 2; x++) {
    for (int y = -1; y < 2; y++) {
      for (int z = -1; z < 2; z++) {
        for (int w = -1; w < 2; w++) {
          if (!((x == 0) && (y == 0) && (z == 0) && (w == 0))) {
            ps.push_back({p.x + x, p.y + y, p.z + z, p.w + w});
          }
        }
      }
    }
  }
  return ps;
}

template <typename Point> struct Universe {
public:
  Universe(const vector<vector<char>> &init) : universe{} {
    for (size_t y = 0; y < init.size(); y++) {
      for (size_t x = 0; x < init.at(y).size(); x++) {
        if (init.at(y).at(x) == '#') {
          universe.insert({static_cast<int>(x), static_cast<int>(y)});
        }
      }
    }
  }

  void printLayer(int z, int w) const {
    auto xCmp = [](const auto &p1, const auto &p2) { return p1.x < p2.x; };
    auto yCmp = [](const auto &p1, const auto &p2) { return p1.y < p2.y; };
    int xBegin = std::min_element(universe.cbegin(), universe.cend(), xCmp)->x;
    int xEnd =
        std::max_element(universe.cbegin(), universe.cend(), xCmp)->x + 1;
    int yBegin = std::min_element(universe.cbegin(), universe.cend(), yCmp)->y;
    int yEnd =
        std::max_element(universe.cbegin(), universe.cend(), yCmp)->y + 1;

    vector<vector<char>> plane;
    for (int y = yBegin; y < yEnd; y++) {
      vector<char> row(xEnd - xBegin);
      std::fill(row.begin(), row.end(), '.');
      plane.push_back(std::move(row));
    }
    for (const auto &p : universe) {
      if ((p.z == z) && (p.w == w)) {
        plane.at(p.y - yBegin).at(p.x - xBegin) = '#';
      }
    }
    printMatrix(plane);
  }

  void runCycle() {
    // 2 things: nodes to create, nodes to destroy
    // creating nodes mean iterating all the neighbors of the current points
    // destroying current nodes means iterating current nodes

    unordered_set<Point> newUniverse;
    unordered_set<Point> pointsCheckedCreation;
    for (const auto &p : universe) {
      // create new blocks
      auto ns = neighbors(p);
      for (const auto &neighbor : ns) {
        // check not a current active point
        bool isActive = universe.find(neighbor) != universe.cend();
        if (!isActive) {
          bool isChecked = (pointsCheckedCreation.find(neighbor) !=
                            pointsCheckedCreation.cend()) ||
                           (newUniverse.find(neighbor) != newUniverse.cend());
          if (!isChecked) {
            int activeNeighborCount = countActiveNeighbors(neighbor);
            if (activeNeighborCount == 3) {
              newUniverse.insert(neighbor);
            } else {
              pointsCheckedCreation.insert(neighbor);
            }
          }
        }
      }
      // destroy old blocks
      int activeNeighborCount = countActiveNeighbors(p);
      if ((activeNeighborCount == 2) || (activeNeighborCount == 3)) {
        newUniverse.insert(p);
      }
    }
    universe = newUniverse;
  }

  std::size_t size() const { return universe.size(); }

private:
  int countActiveNeighbors(const Point &p) const {
    auto ns = neighbors(p);
    return accumulate(ns.cbegin(), ns.cend(), 0,
                      [this](int acc, const auto &n) {
                        if (universe.find(n) != universe.cend()) {
                          return acc + 1;
                        }
                        return acc;
                      });
  }

  unordered_set<Point> universe;
};

void part1(const vector<string> &lines) {
  auto initUniverse = parseMatrixChar(lines);
  Universe<Point3D> universe{initUniverse};
  for (int i = 0; i < 6; i++) {
    universe.runCycle();
  }
  print(universe.size());
}

void part2(const vector<string> &lines) {
  auto initUniverse = parseMatrixChar(lines);
  Universe<Point4D> universe{initUniverse};
  for (int i = 0; i < 6; i++) {
    universe.runCycle();
  }
  print(universe.size());
}

int main() {
  auto lines = readLinesFromFile("input/day17.txt");
  part1(lines);
  part2(lines);
}
