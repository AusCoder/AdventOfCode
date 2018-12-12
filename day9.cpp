#include <cmath>
#include <cstdio>
#include <vector>
#include <deque>
#include <set>
#include <map>
#include <iostream>
#include <algorithm>
#include <cassert>

// This doesnt work, see the python solution
// I cant find where it differs from the python solution
int part1(int numPlayers, int maxMarble) {
    std::deque<int> marbles;
    std::map<int,int> scores = {};
    for (int i=1; i<=numPlayers; i++) {
        scores[i] = 0;
    }
    marbles.push_back(0);

    int cur = 0;
    int curP = 1;
    // int nextMarble = 1;
    // while (nextMarble <= maxMarble) {
    for (int curM=1; curM<=maxMarble; curM++) {
        if (curM % 23 == 0) {
            int remIdx = (cur - 7) % marbles.size();
            scores[curP] += curM;
            scores[curP] += marbles[remIdx];
            marbles.erase(marbles.begin() + remIdx);
            cur = remIdx;
            std::cout << remIdx << ", " << marbles.size() << std::endl;
        } else {
            cur = (cur + 2) % marbles.size();
            marbles.insert(marbles.begin() + cur, curM);
        }

        // if (nextMarble <= 25) {
        //     std::cout << curP << "   " << cur << "   " ;
        //     for (auto &it: marbles) {
        //         std::cout << it << " ";
        //     }
        //     std::cout << std::endl;
        // }


        curP++;
        if (curP > numPlayers) {
            curP = 1;
        }
    }

    int maxScore = 0;
    for (auto &it: scores) {
        // std::cout << it.first << "   " << it.second << std::endl;
        if (it.second > maxScore) {
            maxScore = it.second;
        }
    }

    std::cout << "max score: " << maxScore << std::endl;
    return maxScore;
}

void part2(std::vector<int> &inputs) {
}

int main() {
    part1(9, 25);
    // part1(463, 71787);
    assert(part1(10, 1618) == 8317);
    // assert(part1(13, 7999) == 146373);
    // assert(part1(17, 1104) == 2764);
    // assert(part1(21, 6111) == 54718);
    // assert(part1(30, 5807) == 37305);
    // part2(inputs);
    return 0;
}
