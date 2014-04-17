#include "util.h"

int main(int argc, char** argv) {
  int64_t n, i;
  logging::core::get()->set_filter(
    logging::trivial::severity >= logging::trivial::info);

  if (argc <= 1) {
    n =  600851475143LL;
  } else {
    n = strtoull(argv[1], nullptr, 10);
  }

  Primes primes_(std::sqrt(n));
  vector<int64_t> primes = primes_.Get();

  for (vector<int64_t>::reverse_iterator rit = primes.rbegin();
       rit != primes.rend();
       ++rit) {
    if (n % *rit == 0) {
      cout << "Greatest factor for " << n << " = " << *rit << endl;
      return 0;
    }
  }
  cout << "Bug: No prime factors." << endl;
}
