#include <iostream>
#include <cstdlib>

void part1() {
    int *x = (int *)malloc(sizeof(int) * 50000000000);

    std::cout << "yoyoyo" << std::endl;
}

int main() {
    part1();
    return 0;
}
