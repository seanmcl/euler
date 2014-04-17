#include "util.h"

Primes::Primes(int64_t upto) {
  BOOST_LOG_TRIVIAL(info) << "Computing primes up to " << upto;
  int64_t i;
  for (i = 2; i <= upto; ++i) {
    if (IsPrime(i)) {
      primes_.push_back(i);
    }
  }
}

const vector<int64_t>& Primes::Get() const { return primes_; }

bool Primes::IsPrime(int64_t n) {
  int64_t n2 = sqrt(n);
  for (int64_t i : primes_) {
    if (i > n2) break;
    if (n % i == 0) return false;
  }
  return true;
}
