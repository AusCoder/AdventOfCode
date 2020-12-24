#include "bits-and-bobs.hh"

using namespace std;

#define P2_MAX_VALUE 1000000
#define P2_ARR_SIZE 1000001
#define P2_NUM_ROUNDS 10000000

template <int N> void printGame(const array<int, N> &arr) {
  int startIdx = 1;
  string sep{""};
  cout << arr[startIdx] << sep;
  while ((startIdx = arr[startIdx]) != 1) {
    cout << arr[startIdx] << sep;
  }
  cout << "\n";
}

template <int N> void playGame(array<int, N> &arr, int current, int numRounds) {
  array<int, 3> next;
  int maxValue = *std::max_element(arr.cbegin(), arr.cend());
  for (int roundNum = 0; roundNum < numRounds; roundNum++) {
    // next 3
    int currentIdx = current;
    for (int i = 0; i < 3; i++) {
      next[i] = currentIdx = arr[currentIdx];
    }
    int nextCurrent = arr[currentIdx];
    arr[current] = nextCurrent;

    int destination = current >= 2 ? current - 1 : maxValue;
    while (std::find(next.cbegin(), next.cend(), destination) != next.cend()) {
      destination = destination >= 2 ? destination - 1 : maxValue;
    }

    int destinationEnd = arr[destination];
    int destIdx = destination;
    for (int i = 0; i < 3; i++) {
      arr[destIdx] = next[i];
      destIdx = next[i];
    }
    arr[destIdx] = destinationEnd;
    current = nextCurrent;
  }
}

/*
  Key insight (thanks to reddit)
  is to represent a circle of numbers s_i
  in an array with arr[s_i] = s_{i+1}
*/
void part1() {
  array<int, 10> cups;
  cups[0] = 0;
  cups[6] = 2;
  cups[2] = 4;
  cups[4] = 3;
  cups[3] = 9;
  cups[9] = 7;
  cups[7] = 1;
  cups[1] = 5;
  cups[5] = 8;
  cups[8] = 6;
  // cups[3] = 8;
  // cups[8] = 9;
  // cups[9] = 1;
  // cups[1] = 2;
  // cups[2] = 5;
  // cups[5] = 4;
  // cups[4] = 6;
  // cups[6] = 7;
  // cups[7] = 3;

  playGame<10>(cups, 6, 100);
  printGame<10>(cups);
}

void part2() {
  array<int, P2_ARR_SIZE> cups;
  cups[0] = 0;
  cups[6] = 2;
  cups[2] = 4;
  cups[4] = 3;
  cups[3] = 9;
  cups[9] = 7;
  cups[7] = 1;
  cups[1] = 5;
  cups[5] = 8;
  cups[8] = 10;
  cups[P2_MAX_VALUE] = 6;

  // cups[3] = 8;
  // cups[8] = 9;
  // cups[9] = 1;
  // cups[1] = 2;
  // cups[2] = 5;
  // cups[5] = 4;
  // cups[4] = 6;
  // cups[6] = 7;
  // cups[7] = 10;
  // cups[P2_MAX_VALUE] = 3;

  int startValue = 6;

  for (int i = 10; i < P2_MAX_VALUE; i++) {
    cups[i] = i + 1;
  }

  playGame<P2_ARR_SIZE>(cups, startValue, P2_NUM_ROUNDS);
  long product = static_cast<long>(cups[1]) * static_cast<long>(cups[cups[1]]);
  print(product);
}

int main() {
  part1();
  part2();
}
