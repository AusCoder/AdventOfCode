#include <cmath>
#include <cstdio>
#include <vector>
#include <set>
#include <iostream>
#include <algorithm>

class Node {
    public:
        std::vector<Node*> children;
        std::vector<int> metadata;
        Node(std::vector<Node*> c): children(c) {};
};

Node *readNode() {
    int numChildren;
    int numMetadata;
    Node *n;

    std::cin >> numChildren;
    std::cin >> numMetadata;
    std::vector<Node*> c = {};
    int metadata;
    while (numChildren-- > 0) {
        c.push_back(readNode());
    }
    n = new Node(c);
    while (numMetadata-- > 0) {
        std::cin >> metadata;
        n->metadata.push_back(metadata);
    }
    return n;
}

int SumMetadata(const Node *node) {
    int total = 0;
    for (auto &it: node->children) {
        total += SumMetadata(it);
    }
    for (auto &it: node->metadata) {
        total += it;
    }
    return total;
}

void part1(const Node *node) {
    std::cout << "sum metadata: " << SumMetadata(node) << std::endl;
}

int CalculateValue(const Node *node) {
    int value = 0;
    if (node->children.size() == 0) {
        for (auto &it: node->metadata) {
            value += it;
        }
        return value;
    }
    std::vector<int> childValues = {};
    for (auto &it: node->children) {
        childValues.push_back(CalculateValue(it));
    }
    for (auto &it: node->metadata) {
        int idx = it - 1;
        if (idx < 0 || idx >= childValues.size()) {
            continue;
        }
        value += childValues.at(idx);
    }
    return value;
}

void part2(const Node *node) {
    std::cout << "value of root: " << CalculateValue(node) << std::endl;
}

void PrintNode(const Node *node) {
    std::cout << node->children.size() << " " << node->metadata.size() << " ";
    for (auto &it: node->children) {
        PrintNode(it);
    }
    for (auto &it: node->metadata) {
        std::cout << it << " ";
    }
}

void DeleteNodes(const Node *node) {
    for (auto &it: node->children) {
        DeleteNodes(it);
    }
    delete node;
}

int main() {
    Node *startNode = readNode();

// debug
    // PrintNode(startNode);

    part1(startNode);
    part2(startNode);

    DeleteNodes(startNode);
    return 0;
}
