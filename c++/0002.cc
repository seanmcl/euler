#include "util.h"

int main(int argc, char** argv) {
  int64_t n1(1), n2(1), sum(0), max(0);

  if (argc <= 1) {
    max = 4000000LL;
  } else {
    max = strtoull(argv[1], nullptr, 10);
  }

  while (n2 <= max) {
    if (n2 % 2 == 0) sum += n2;
    int64_t tmp = n1;
    n1 = n2;
    n2 += tmp;
  }

  cout << "max: " << max << endl;
  cout << "sum: " << sum << endl;
  return 0;
}
