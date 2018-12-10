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

    thoughts:
        - I spent a long while thinking about geomtry of boundaries in the manhattan space
        - but sometimes, just brute forcing it is the way to make progress!
*/

class Point {
    public:
        int x;
        int y;
    Point(int a, int b): x(a), y(b) {};
};

int distance(const Point &p1, const Point &p2) {
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
void RenderMap(const std::vector<std::vector<int>> pointMap) {
    std::string letters = "abcdefgihjklmnop";
    char c;
    for (auto &jt: pointMap) {
        for (auto &it: jt) {
            if (it < 0) {
                c = '.';
            } else {
                c = letters[it];
            }
            std::cout << c;
        }
        std::cout << std::endl;
    }
}

int MaxArea(const std::vector<std::vector<int>> pointMap) {
    std::set<int> edgeLabels = {};
    int jdx, idx;

    for (idx=0; idx<pointMap[0].size(); idx++) {
        jdx = 0;
        edgeLabels.insert(pointMap[jdx][idx]);
        jdx = pointMap.size() - 1;
        edgeLabels.insert(pointMap[jdx][idx]);
    }

    for (jdx=0; jdx<pointMap.size(); jdx++) {
        idx = 0;
        edgeLabels.insert(pointMap[jdx][idx]);
        idx = pointMap[jdx].size() - 1;
        edgeLabels.insert(pointMap[jdx][idx]);
    }

    std::map<int,int> classSizes;
    for (auto &jt: pointMap) {
        for (auto &it: jt) {
            auto foundOnEdge = edgeLabels.find(it);
            if (foundOnEdge != edgeLabels.end()) {
                continue;
            }
            auto found = classSizes.find(it);
            if (found == classSizes.end()) {
                classSizes[it] = 0;
            }
            classSizes[it]++;
        }
    }
    int maxArea = 0;
    for (auto &it: classSizes) {
        if (it.second > maxArea) {
            maxArea = it.second;
        }
    }
    return maxArea;
}

void part1(const std::vector<Point> &input) {
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
    // std::cout << minX << " " << maxX << std::endl;
    // std::cout << minY << " " << maxY << std::endl;

    std::vector<std::vector<int>> map;
    int nearestNeighbour;
    int minDist = INT_MAX;
    int minDist2nd = INT_MAX;
    for (int j=minY; j<=maxY; j++) {
        map.push_back(std::vector<int>());
        for (int i=minX; i<=maxX; i++) {
            for (int k=0; k<input.size(); k++) {
                const Point &it = input[k];
                Point p = Point(i, j);
                int d = distance(it, p);
                if (d < minDist) {
                    minDist2nd = minDist;
                    minDist = d;
                    nearestNeighbour = k;
                } else if (d < minDist2nd) {
                    minDist2nd = d;
                }
            }
            if (minDist != minDist2nd) {
                map[j - minY].push_back(nearestNeighbour);
            } else {
                map[j - minY].push_back(-1);
            }
            nearestNeighbour = -1;
            minDist = INT_MAX;
            minDist2nd = INT_MAX;
        }
    }
    // RenderMap(map);
    std::cout << "max area is: " << MaxArea(map) << std::endl;
}

// part2
void part2(const std::vector<Point> &input, int distThreshold) {
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

    std::vector<std::vector<int>> pointMap;
    int pointDist;
    for (int j=minY; j<=maxY; j++) {
        pointMap.push_back(std::vector<int>());
        for (int i=minX; i<=maxX; i++) {
            pointDist = 0;
            for (int k=0; k<input.size(); k++) {
                const Point &it = input[k];
                Point p = Point(i, j);
                pointDist += distance(it, p);
            }
            if (pointDist < distThreshold) {
                pointMap[j - minY].push_back(1);
            } else {
                pointMap[j - minY].push_back(0);
            }
        }
    }
    // RenderMap(pointMap);
    int totalArea = 0;
    for (auto &jt: pointMap) {
        for (auto &it: jt) {
            totalArea += it;
        }
    }
    std::cout << "area of safe region is: " << totalArea << std::endl;
}

int main() {
    std::vector<Point> input = read_input();

    part1(input);
    part2(input, 10000);
    return 0;
}
