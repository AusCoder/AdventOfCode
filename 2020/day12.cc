#include "bits-and-bobs.hh"

using namespace std;

struct FerryPart1 {
  int x;
  int y;
  int direction;

  FerryPart1() : x{0}, y{0}, direction{90} {}

  void nagivate(const string &s) {
    int num = std::stoi(s.substr(1, s.size()));
    switch (s.at(0)) {
    case 'F':
      switch (direction) {
      case 0:
        y += num;
        break;
      case 90:
        x += num;
        break;
      case 180:
        y -= num;
        break;
      case 270:
        x -= num;
        break;
      default:
        throw std::runtime_error("invalid direction");
      }
      break;

    case 'R':
      direction += num + 360;
      direction %= 360;
      break;

    case 'L':
      direction += 360 - num;
      direction %= 360;
      break;

    case 'N':
      y += num;
      break;
    case 'E':
      x += num;
      break;
    case 'S':
      y -= num;
      break;
    case 'W':
      x -= num;
      break;
    }
  }

  int distanceFromOrigin() const { return std::abs(x) + std::abs(y); }
};

struct FerryPart2 {
  int shipX;
  int shipY;
  int waypointX;
  int waypointY;

  FerryPart2() : shipX{0}, shipY{0}, waypointX{10}, waypointY{1} {}

  void navigate(const string &s) {
    int num = std::stoi(s.substr(1, s.size()));
    switch (s.at(0)) {
    case 'F':
      shipX += num * waypointX;
      shipY += num * waypointY;
      break;
    case 'R':
      rotateWaypoint(num);
      break;
    case 'L':
      rotateWaypoint(360 - num);
      break;
    case 'N':
      waypointY += num;
      break;
    case 'E':
      waypointX += num;
      break;
    case 'S':
      waypointY -= num;
      break;
    case 'W':
      waypointX -= num;
      break;
    default:
      throw std::runtime_error("invalid action");
    }
  }

  void rotateWaypoint(int degrees) {
    int tmp;
    switch (degrees) {
    case 0:
      break;
    case 90:
      tmp = waypointX;
      waypointX = waypointY;
      waypointY = -tmp;
      break;
    case 180:
      waypointX = -waypointX;
      waypointY = -waypointY;
      break;
    case 270:
      tmp = waypointX;
      waypointX = -waypointY;
      waypointY = tmp;
      break;
    default:
      throw std::runtime_error("invalid degrees");
    }
  }

  int distanceFromOrigin() const { return std::abs(shipX) + std::abs(shipY); }
};

void part1(const vector<string> &lines) {
  FerryPart1 ferry;
  std::for_each(lines.cbegin(), lines.cend(),
                [&](const auto &s) { ferry.nagivate(s); });
  print(ferry.distanceFromOrigin());
}

void part2(const vector<string> &lines) {
  FerryPart2 ferry;
  std::for_each(lines.cbegin(), lines.cend(),
                [&](const auto &s) { ferry.navigate(s); });
  print(ferry.distanceFromOrigin());
}

int main() {
  auto lines = readLinesFromFile("input/day12.txt");
  part1(lines);
  part2(lines);
}
