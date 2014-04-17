#ifndef UTIL_H_
#define UTIL_H_

#include <algorithm>
#include <ctgmath>
#include <iostream>
#include <stdint.h>
#include <stdlib.h>
#include <string>
#include <vector>

#include <boost/log/core.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/trivial.hpp>

namespace logging = boost::log;

using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::vector;

class Primes {
 public:
  explicit Primes(int64_t upto);
  const vector<int64_t>& Get() const;

 private:
  vector<int64_t> primes_;
  bool IsPrime(int64_t n);
};

int64_t LargestFactor(int64_t n) {

}

#endif  // UTIL_H_
