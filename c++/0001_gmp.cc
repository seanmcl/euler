// This version is ~20X slower than the int64_t version.

#include "gmpxx.h"
#include "util.h"

int main(int argc, char** argv) {
  string maxs;
  mpz_class max, sum, i;
  if (argc <= 1) {
    maxs = "1000";
  } else {
    max = argv[1];
  }
  if (!max.set_str(maxs, 10)) {
    cerr << "Error: can't parse " << maxs << endl;
    return(1);
  }

  i.set_str("0", 10);
  for (; i < max; ++i) {
    if (i % 3 == 0 || i % 5 == 0) sum += i;
  }
  cout << "max: " << max << endl;
  cout << "sum: " << sum << endl;
  return 0;
}
