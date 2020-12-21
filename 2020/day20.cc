#include "bits-and-bobs.hh"
#include <optional>
#include <unordered_set>

using namespace std;

string reverseStr(const string &s) {
  string out;
  transform(s.rbegin(), s.rend(), back_inserter(out),
            [](const auto &c) { return c; });
  return out;
}

struct Tile {
  int id;
  vector<string> content;

  void rotateCounterClockwise() {
    vector<string> rotated;
    for (int i = content.at(0).size() - 1; i >= 0; i--) {
      string s;
      transform(content.cbegin(), content.cend(), back_inserter(s),
                [&i](const auto &row) { return row.at(i); });
      rotated.push_back(std::move(s));
    }
    content = std::move(rotated);
  }

  void flipLeftRight() {
    for_each(content.begin(), content.end(),
             [](auto &s) { reverse(s.begin(), s.end()); });
  }

  string getBorder(int idx) const {
    string s;
    size_t lineSize = content.at(0).size();
    switch (idx) {
    case 0: // top
      s = content.at(0);
      break;
    case 1: // right
      transform(content.cbegin(), content.cend(), back_inserter(s),
                [&lineSize](const string &line) {
                  assert(line.size() == lineSize);
                  return line.at(line.size() - 1);
                });
      break;
    case 2: // bottom
      s = content.at(content.size() - 1);
      // std::reverse(s.begin(), s.end());
      break;
    case 3: // left
      transform(content.cbegin(), content.cend(), back_inserter(s),
                [&lineSize](const string &line) {
                  assert(line.size() == lineSize);
                  return line.at(0);
                });
      // std::reverse(s.begin(), s.end());
      break;
    default:
      throw std::runtime_error("invalid border idx");
    }
    assert(s.size() == lineSize);
    return s;
  }

  int findMatchingSide(const Tile &other) const {
    for (int i = 0; i < 4; i++) {
      string otherBorder = other.getBorder((i + 2) % 4);
      // std::reverse(otherBorder.begin(), otherBorder.end());
      if (getBorder(i) == otherBorder) {
        return i;
      }
    }
    return -1;
  }

  vector<string> getAllBorders() const {
    vector<string> bs;
    for (int i = 0; i < 4; i++) {
      bs.push_back(std::move(getBorder(i)));
      bs.push_back(reverseStr(bs.at(bs.size() - 1)));
    }
    return bs;
  }

  vector<string> getContentWithoutBorder() const {
    vector<string> contentWithoutBorder;
    transform(content.cbegin() + 1, content.cend() - 1,
              back_inserter(contentWithoutBorder), [](const string &l) {
                assert(l.size() > 2);
                return l.substr(1, l.size() - 2);
              });
    return contentWithoutBorder;
  }
};

void printTile(const Tile &tile) {
  for_each(tile.content.cbegin(), tile.content.cend(),
           [](const auto &s) { cout << s << "\n"; });
}

vector<Tile> parseTiles(const vector<string> &lines) {
  auto it = lines.cbegin();

  vector<Tile> tiles;
  for (; it != lines.cend(); it++) {
    // Read id
    std::regex idRe("Tile\\s(\\d+):");
    std::smatch idMatch;
    std::regex_match(*it, idMatch, idRe);
    assert(!idMatch.empty());
    int id = std::stoi(idMatch.str(1));
    it++;
    // Read content
    vector<string> content;
    while (!it->empty()) {
      content.push_back(*it++);
    }
    tiles.emplace_back(id, std::move(content));
    if (it == lines.cend()) {
      break;
    }
  }
  return tiles;
}

vector<int> findCornerTileIds(const vector<Tile> &tiles) {
  vector<int> cornerTileIds;
  for (const auto &tile : tiles) {
    int neighborCount = 0;
    for (const auto &otherTile : tiles) {
      if (otherTile.id != tile.id) {
        int matchCount = 0;
        auto neighborBorders = otherTile.getAllBorders();
        for (int i = 0; i < 4; i++) {
          string border = tile.getBorder(i);
          bool isMatch =
              std::any_of(neighborBorders.cbegin(), neighborBorders.cend(),
                          [&border](const auto &nb) { return border == nb; });
          if (isMatch) {
            matchCount++;
          }
        }
        assert((matchCount == 0) || (matchCount == 1));
        neighborCount += matchCount;
      }
    }
    if (neighborCount == 2) {
      cornerTileIds.push_back(tile.id);
    }
  }
  assert(cornerTileIds.size() == 4);
  return cornerTileIds;
}

struct NeighborMatch {
  int neighborId;
  int sideIdx;
};

std::optional<int> findNeighborBySideIdx(const vector<NeighborMatch> &neighbors,
                                         int sideIdx) {
  std::optional<int> neighborIdx = std::nullopt;
  for (const NeighborMatch &match : neighbors) {
    if (match.sideIdx == sideIdx) {
      neighborIdx = match.neighborId;
    }
  }
  return neighborIdx;
}

int countHashes(const vector<string> &lines) {
  return std::accumulate(
      lines.cbegin(), lines.cend(), 0, [](auto acc, const string &line) {
        return acc + std::accumulate(line.cbegin(), line.cend(), 0,
                                     [](auto ac, const char &c) {
                                       if (c == '#') {
                                         return ac + 1;
                                       }
                                       return ac;
                                     });
      });
}

int countSeaMonsterHabitatOnFixedTile(const Tile &imageTile) {
  vector<string> seaMonster{"                  # ", "#    ##    ##    ###",
                            " #  #  #  #  #  #   "};
  size_t imageHeight = imageTile.content.size();
  size_t imageWidth = imageTile.content.at(0).size();
  size_t seaMonsterHeight = seaMonster.size();
  size_t seaMonsterWidth = seaMonster.at(0).size();
  // int habitatCount = 0;
  int seaMonsterCount = 0;
  for (size_t rowIdx = 0; rowIdx < imageHeight - seaMonsterHeight + 1;
       rowIdx++) {
    for (size_t colIdx = 0; colIdx < imageWidth - seaMonsterWidth + 1;
         colIdx++) {
      int posHabitatCount = 0;
      // check sea monster
      for (size_t seaRowIdx = 0; seaRowIdx < seaMonsterHeight; seaRowIdx++) {
        for (size_t seaColIdx = 0; seaColIdx < seaMonsterWidth; seaColIdx++) {
          char seaChar = seaMonster.at(seaRowIdx).at(seaColIdx);
          char imageChar =
              imageTile.content.at(rowIdx + seaRowIdx).at(colIdx + seaColIdx);
          if ((seaChar == ' ') && (imageChar == '#')) {
            // habitat
            posHabitatCount++;
          } else if ((seaChar == '#') && (imageChar != '#')) {
            // not sea monster
            posHabitatCount = -1;
            break;
          }
        }
        if (posHabitatCount == -1) {
          break;
        }
      }
      if (posHabitatCount > 0) {
        seaMonsterCount++;
      }
    }
  }

  int seaMonsterHashCount = countHashes(seaMonster);
  int imageHashCount = countHashes(imageTile.content);

  int result = 0;
  if (seaMonsterCount > 0) {
    result = imageHashCount - (seaMonsterCount * seaMonsterHashCount);
  }
  return result;
}

int countSeaMonsterHabitat(Tile &imageTile) {
  int habitatCount = 0;
  for (int i = 0; i < 4; i++) {
    habitatCount = countSeaMonsterHabitatOnFixedTile(imageTile);
    if (habitatCount != 0) {
      break;
    }
    imageTile.rotateCounterClockwise();
  }
  if (habitatCount == 0) {
    imageTile.flipLeftRight();
    for (int i = 0; i < 4; i++) {
      habitatCount = countSeaMonsterHabitatOnFixedTile(imageTile);
      if (habitatCount != 0) {
        break;
      }
      imageTile.rotateCounterClockwise();
    }
  }
  return habitatCount;
}

void part2(const vector<string> &lines) {
  auto tiles = parseTiles(lines);
  auto cornerTileIds = findCornerTileIds(tiles);

  unordered_map<int, Tile> tilesById;
  unordered_map<int, vector<NeighborMatch>> neighbors;
  for (const auto &tile : tiles) {
    tilesById.insert({tile.id, tile});
  }
  neighbors.insert({cornerTileIds.at(0), {}});

  // Solve the puzzle by rotating and matching borders
  while (neighbors.size() < tilesById.size()) {
    for (auto &p : tilesById) {
      // if not placed, try placing it next to some existing tile
      if (neighbors.find(p.first) == neighbors.end()) {
        Tile &tile = p.second;
        for (const auto &possibleNeighborPair : neighbors) {
          const Tile &possibleNeighbor =
              tilesById.at(possibleNeighborPair.first);

          // for each rotation and flip
          int matchingSideIdx = -1;
          for (int i = 0; i < 4; i++) {
            matchingSideIdx = possibleNeighbor.findMatchingSide(tile);
            if (matchingSideIdx != -1) {
              break;
            }
            tile.rotateCounterClockwise();
          }
          if (matchingSideIdx == -1) {
            tile.flipLeftRight();
            for (int i = 0; i < 4; i++) {
              matchingSideIdx = possibleNeighbor.findMatchingSide(tile);
              if (matchingSideIdx != -1) {
                break;
              }
              tile.rotateCounterClockwise();
            }
          }
          if (matchingSideIdx != -1) {
            int matchingSizeIdxRelTile = (matchingSideIdx + 2) % 4;
            assert(tile.findMatchingSide(possibleNeighbor) ==
                   matchingSizeIdxRelTile);
            neighbors[possibleNeighbor.id].push_back(
                {tile.id, matchingSideIdx});
            neighbors[tile.id].push_back(
                {possibleNeighbor.id, matchingSizeIdxRelTile});
            break;
          }
        }
        // find other neighbors of tile without modifying it
        const Tile &ctile = tile;
        for (const auto &possibleNeighborPair : neighbors) {
          const Tile &possibleNeighbor =
              tilesById.at(possibleNeighborPair.first);
          int matchingSideIdx = possibleNeighbor.findMatchingSide(ctile);
          if (matchingSideIdx != -1) {
            bool matchExists = std::any_of(
                neighbors[possibleNeighbor.id].cbegin(),
                neighbors[possibleNeighbor.id].cend(),
                [&ctile](const auto &m) { return m.neighborId == ctile.id; });
            if (!matchExists) {
              int matchingSizeIdxRelTile = (matchingSideIdx + 2) % 4;
              assert(tile.findMatchingSide(possibleNeighbor) ==
                     matchingSizeIdxRelTile);
              neighbors[possibleNeighbor.id].push_back(
                  {ctile.id, matchingSideIdx});
              neighbors[tile.id].push_back(
                  {possibleNeighbor.id, matchingSizeIdxRelTile});
            }
          }
        }
      }
    }
  }

  // // print neighbors
  // for (const auto &neighborPair : neighbors) {
  //   cout << neighborPair.first << ": ";
  //   for_each(neighborPair.second.cbegin(), neighborPair.second.cend(),
  //            [](const auto &i) { cout << i.neighborId << " "; });
  //   cout << "\n";
  // }

  // Create the solved grid
  int topLeftId = -1;
  for (const auto &neighborPair : neighbors) {
    if (neighborPair.second.size() == 2) {
      const NeighborMatch &m0 = neighborPair.second.at(0);
      const NeighborMatch &m1 = neighborPair.second.at(1);
      bool isTopLeft = ((m0.sideIdx == 1) && (m1.sideIdx == 2)) ||
                       ((m0.sideIdx == 2) && (m1.sideIdx == 1));
      if (isTopLeft) {
        topLeftId = neighborPair.first;
        break;
      }
    }
  }
  assert(topLeftId != -1);

  // Assemble idGrid
  vector<vector<int>> idGrid;
  // push rows
  for (;;) {
    // push a row
    int leftMostId = topLeftId;
    idGrid.push_back({});
    vector<int> &currentRow = idGrid.at(idGrid.size() - 1);
    currentRow.push_back(leftMostId);
    for (;;) {
      const vector<NeighborMatch> &leftMostNeighbors = neighbors.at(leftMostId);
      optional<int> rightNeighbor = findNeighborBySideIdx(leftMostNeighbors, 1);
      if (rightNeighbor.has_value()) {
        currentRow.push_back(rightNeighbor.value());
        leftMostId = rightNeighbor.value();
      } else {
        break;
      }
    }

    const vector<NeighborMatch> &topLeftNeighbors = neighbors.at(topLeftId);
    optional<int> rowBelowId = findNeighborBySideIdx(topLeftNeighbors, 2);
    if (rowBelowId.has_value()) {
      topLeftId = rowBelowId.value();
    } else {
      break;
    }
  }

  // Build image
  vector<string> contentWithoutBorder0 =
      tilesById.at(idGrid.at(0).at(0)).getContentWithoutBorder();
  assert(contentWithoutBorder0.size() == contentWithoutBorder0.at(0).size());
  int contentWithoutBorderSize = contentWithoutBorder0.size();

  vector<string> image(contentWithoutBorderSize * idGrid.size());
  for (size_t blockRowIdx = 0; blockRowIdx < idGrid.size(); blockRowIdx++) {
    const auto &idRow = idGrid.at(blockRowIdx);
    for (const auto &id : idRow) {
      int imageRowIdx = contentWithoutBorderSize * blockRowIdx;
      const Tile &tile = tilesById.at(id);
      vector<string> contentWithoutBorder = tile.getContentWithoutBorder();
      for (auto &s : contentWithoutBorder) {
        image.at(imageRowIdx++).append(s);
      }
    }
  }

  // Search for sea monsters!
  Tile imageTile{1, image};
  int habitatCount = countSeaMonsterHabitat(imageTile);
  print(habitatCount);
}

void part1(const vector<string> &lines) {
  auto tiles = parseTiles(lines);
  auto cornerTileids = findCornerTileIds(tiles);
  long idProduct =
      accumulate(cornerTileids.cbegin(), cornerTileids.cend(), 1L,
                 [](const auto &acc, const auto &id) { return acc * id; });
  print(idProduct);
}

int main() {
  auto lines = readLinesFromFile("input/day20.txt");
  part1(lines);
  part2(lines);
}
