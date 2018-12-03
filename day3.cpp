#include <cstdio>
#include <vector>
#include <iostream>
#include <string>
#include <map>
#include <tuple>
#include <cassert>
#include <regex>

struct Rect {
    int id;

    int leftDelta;
    int topDelta;

    int width;
    int height;

    Rect(int i, int l, int t, int w, int h): id(i), leftDelta(l), topDelta(t), width(w), height(h) {};
};

std::vector<Rect> read_input() {
    std::vector<Rect> vals;
    std::string input;
    std::cmatch groups;
    std::regex txt_regex("^#(\\d+)\\ @\\ (\\d+)\\,(\\d+):\\ (\\d+)x(\\d+)$");

    while (getline(std::cin, input)) {
        bool res = std::regex_match(input.c_str(), groups, txt_regex);
        assert(res);
        Rect rect = Rect(
            std::stoi(groups[1]),
            std::stoi(groups[2]),
            std::stoi(groups[3]),
            std::stoi(groups[4]),
            std::stoi(groups[5]));
        vals.push_back(rect);
    }

    return vals;
}

// part 1
void MarkPoint(const Rect &rect, std::vector<std::vector<int>> &pointCounts) {
    for (int y=0; y<rect.height; y++) {
        for (int x=0; x<rect.width; x++) {
            int posY = rect.topDelta + y;
            int posX = rect.leftDelta + x;
            pointCounts[posY][posX]++;
        }
    }
}

std::vector<std::vector<int>> BuildPointCounts(std::vector<Rect> inputs) {
    int curHeight = 0;
    int curWidth = 0;
    std::vector<std::vector<int>> pointCounts;

    for (auto it=inputs.begin(); it!=inputs.end(); it++) {
        int maxX = it->leftDelta + it->width;
        int maxY = it->topDelta + it->height;

        // set the height
        while (curHeight < maxY) {
            curHeight++;
            std::vector<int> x;
            for(int i=0; i<curWidth; i++) {
                x.push_back(0);
            }
            pointCounts.push_back(x);
        }

        // set the width
        while (curWidth < maxX) {
            curWidth++;
            for (auto jt=pointCounts.begin(); jt!=pointCounts.end(); jt++) {
                jt->push_back(0);
            }
        }

        // mark values
        MarkPoint(*it, pointCounts);
    }
    return pointCounts;
}

int CountOverlaps(std::vector<std::vector<int>> &pointCounts) {
    int count = 0;
    for (auto it=pointCounts.begin(); it!=pointCounts.end(); it++) {
        for (auto jt=it->begin(); jt!=it->end(); jt++) {
            if (*jt > 1) {
                count++;
            }
        }
    }
    return count;
}

int PrintCounts(std::vector<std::vector<int>> &pointCounts) {
    int count = 0;
    for (auto it=pointCounts.begin(); it!=pointCounts.end(); it++) {
        for (auto jt=it->begin(); jt!=it->end(); jt++) {
            std::cout << *jt;
        }
        std::cout << std::endl;
    }
    return count;
}

void part1(std::vector<Rect> inputs) {
    std::vector<std::vector<int>> pointCounts = BuildPointCounts(inputs);

    assert(pointCounts.size() > 0);
    if (pointCounts.size() > 0) {
        int fstW = pointCounts[0].size();
        for (auto it=pointCounts.begin()+1; it!=pointCounts.end(); it++) {
            assert(fstW == it->size());
        }
    }

    std::cout << "overlaps: " << CountOverlaps(pointCounts) << std::endl;
}

// part2
bool IsRectOverlapping(Rect &rect, std::vector<std::vector<int>> &pointCounts) {
    for (int y=0; y<rect.height; y++) {
        for (int x=0; x<rect.width; x++) {
            int posY = rect.topDelta + y;
            int posX = rect.leftDelta + x;
            if (pointCounts[posY][posX] > 1) {
                return 1;
            }
        }
    }
    return 0;
}

void part2(std::vector<Rect> inputs) {
    std::vector<std::vector<int>> pointCounts = BuildPointCounts(inputs);
    for (auto it=inputs.begin(); it!=inputs.end(); it++) {
        if (!IsRectOverlapping(*it, pointCounts)) {
            std::cout << "not overlapping rect: " << it->id << std::endl;
        }
    }
}

int main() {
    std::vector<Rect> inputs = read_input();
    part1(inputs);
    part2(inputs);
    return 0;
}
