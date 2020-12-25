#include "bits-and-bobs.hh"
#include <unordered_set>

using namespace std;

struct Point {
  int x;
  int y;
};

bool operator==(const Point &p1, const Point &p2) {
  return (p1.x == p2.x) && (p1.y == p2.y);
}

ostream &operator<<(ostream &s, const Point &p) {
  s << "Point(" << p.x << ", " << p.y << ")";
  return s;
}

namespace std {
template <> struct hash<Point> {
  std::size_t operator()(const Point &p) const noexcept {
    return hash<int>()(p.x) ^ (hash<int>()(p.y) << 1);
  }
};
} // namespace std

Point followPath(const string &path, const Point &point) {
  Point p{point};
  for (auto it = path.cbegin(); it != path.cend(); it++) {
    switch (*it) {
    case 'e':
      p.x++;
      break;
    case 'w':
      p.x--;
      break;
    case 's':
      it++;
      assert(it != path.cend());
      switch (*it) {
      case 'e':
        p.y--;
        p.x++;
        break;
      case 'w':
        p.y--;
        break;
      default:
        throw std::runtime_error("invalid char");
      }
      break;
    case 'n':
      it++;
      assert(it != path.cend());
      switch (*it) {
      case 'e':
        p.y++;
        break;
      case 'w':
        p.y++;
        p.x--;
        break;
      default:
        throw std::runtime_error("invalid char");
      }
      break;
    default:
      throw std::runtime_error("invalid char");
    }
  }
  return p;
}

Point followPathFromOrigin(const string &path) {
  Point p{0, 0};
  p = followPath(path, p);
  return p;
}

unordered_set<Point> flipInitialTiles(const vector<string> &lines) {
  unordered_set<Point> blackTiles;
  for (const auto &line : lines) {
    auto p = followPathFromOrigin(line);
    if (blackTiles.find(p) != blackTiles.cend()) {
      blackTiles.erase(p);
    } else {
      blackTiles.insert(p);
    }
  }
  return blackTiles;
}

void part1(const vector<string> &lines) {
  unordered_set<Point> blackTiles = flipInitialTiles(lines);
  print(blackTiles.size());
}

vector<Point> getNeighbors(const Point &p) {
  vector<Point> points;
  vector<string> adjPaths{"e", "ne", "nw", "w", "se", "sw"};
  transform(adjPaths.cbegin(), adjPaths.cend(), back_inserter(points),
            [&p](const string &s) { return followPath(s, p); });
  return points;
}

int countBlackNeighbors(const unordered_set<Point> &blackTiles,
                        const Point &point) {
  vector<Point> neighbors = getNeighbors(point);
  return accumulate(neighbors.cbegin(), neighbors.cend(), 0,
                    [&blackTiles](int acc, const Point &p) {
                      if (blackTiles.find(p) != blackTiles.cend()) {
                        return acc + 1;
                      }
                      return acc;
                    });
}

void part2(const vector<string> &lines) {
  int numTurns = 100;
  unordered_set<Point> blackTiles = flipInitialTiles(lines);
  for (int n = 0; n < numTurns; n++) {
    unordered_set<Point> nextBlackTiles;
    for (const Point &point : blackTiles) {
      // keep black tiles
      int blackNeighborCount = countBlackNeighbors(blackTiles, point);
      if ((blackNeighborCount >= 1) && (blackNeighborCount <= 2)) {
        nextBlackTiles.insert(point);
      }
      // create black tiles
      vector<Point> neighbors = getNeighbors(point);
      unordered_set<Point> seenNeighbors;
      for (const Point &neighbor : neighbors) {
        if ((nextBlackTiles.find(neighbor) == nextBlackTiles.cend()) &&
            (seenNeighbors.find(neighbor) == seenNeighbors.cend())) {
          if (blackTiles.find(neighbor) == blackTiles.cend()) {
            // white tile
            int neighborCount = countBlackNeighbors(blackTiles, neighbor);
            if (neighborCount == 2) {
              nextBlackTiles.insert(neighbor);
            } else {
              seenNeighbors.insert(neighbor);
            }
          }
        }
      }
    }
    // set tiles
    blackTiles = std::move(nextBlackTiles);
  }
  print(blackTiles.size());
}

int main() {
  auto lines = readLinesFromFile("input/day24.txt");
  part1(lines);
  part2(lines);
}
