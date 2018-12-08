#include <vector>
#include <iostream>
#include <string>
#include <map>
#include <set>
#include <tuple>
#include <regex>
#include <cassert>
#include <cctype>
#include <climits>

/*
    idea:
        - find the points with bounded number of neighbours
*/

class Point {
    public:
        int x;
        int y;
    Point(int a, int b): x(a), y(b) {};
};

int distance(Point &p1, Point &p2) {
    return abs(p1.x - p2.x) + abs(p1.y - p2.y);
}

class Rect {
    public:
        Point corner1;
        Point corner2;
    Rect(Point p1, Point p2): corner1(p1), corner2(p2) {};
};

std::vector<Point> read_input() {
    std::vector<Point> vals;
    std::string input;
    std::cmatch groups;
    std::regex txt_regex("^(\\d+),\\ (\\d+)$");

    while (std::getline(std::cin, input)) {
        bool res = std::regex_match(input.c_str(), groups, txt_regex);
        assert(res);
        Point point = Point(std::stoi(groups[1]), std::stoi(groups[2]));
        vals.push_back(point);
    }
    return vals;
}

// part1
void part1(std::vector<Point> &input) {
    int minX = INT_MAX;
    int maxX = INT_MIN;
    int minY = INT_MAX;
    int maxY = INT_MIN;
    for (auto &it: input) {
        if (it.x < minX) {
            minX = it.x;
        }
        if (it.x > maxX) {
            maxX = it.x;
        }
    }
    for (auto &it: input) {
        if (it.y < minY) {
            minY = it.y;
        }
        if (it.y > maxY) {
            maxY = it.y;
        }
    }
    std::cout << minX << " " << maxX << std::endl;
    std::cout << minY << " " << maxY << std::endl;

    std::vector<std::vector<int>> map;
    int nearestNeighbour;
    // int minDist;
    // int minDist2nd;
    for (int j=minY; j<=maxY; j++) {
        map.push_back(std::vector<int>());
        for (int i=minX; i<=maxX; i++) {
            // map[i - minY].push_back(-1);
            for (auto &it: input) {
                Point p = Point(i, j);
                int d = distance(it, p);
            }
        }
    }
    std::cout << "rows: " << map.size() << " cols: " << map[0].size() << std::endl;
}

// part2
void part2(std::vector<Point> &input) {
}

int main() {
    std::vector<Point> input = read_input();

    for (auto &it: input) {
        std::cout << it.x << ", " << it.y << std::endl;
    }

    part1(input);
    // part2(input);
    return 0;
}
