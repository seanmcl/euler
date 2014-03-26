#include "util.h"

int main(int argc, char** argv) {
  int64_t max(0), sum(0), i(0);
  if (argc <= 1) {
    max = 1000LL;
  } else {
    max = strtoull(argv[1], nullptr, 10);
  }
  for (i = 0; i < max; ++i) {
    if (i % 3 == 0 || i % 5 == 0) sum += i;
  }
  cout << "max: " << max << endl;
  cout << "sum: " << sum << endl;
  return 0;
}
