#include <pthread.h>

#include "util.h"

#define NUM_THREADS 5

struct Bounds {
  int threadid;
  int64_t lo;
  int64_t hi;
};

void* Sum(void* bounds_) {
  Bounds* bounds = (Bounds*) bounds_;
  int64_t* sum = new int64_t(0);
  for (int64_t i = bounds->lo; i < bounds->hi; ++i) {
    if (i % 3 == 0 || i % 5 == 0) *sum += i;
  }
  pthread_exit(sum);
}

int main(int argc, char** argv) {
  int64_t max(0);
  if (argc <= 1) {
    max = 1000LL;
  } else {
    max = strtoull(argv[1], nullptr, 10);
  }

  pthread_t threads[NUM_THREADS];

  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

  vector<Bounds> bounds_vec;
  int64_t sums[NUM_THREADS];
  int64_t block_size;
  block_size = max / NUM_THREADS;

  for (int t = 0; t < NUM_THREADS; ++t) {
    Bounds bounds;
    bounds.threadid = t;
    bounds.lo = block_size * t;
    bounds.hi = t == NUM_THREADS - 1 ? max : block_size * (t + 1);
    bounds_vec.push_back(bounds);
  }

  for (int t = 0; t < NUM_THREADS; ++t) {
    pthread_create(&threads[t], &attr, Sum, (void*) &bounds_vec[t]);
  }

  pthread_attr_destroy(&attr);
  int64_t sum(0);
  void* sum1;
  for(int t = 0; t < NUM_THREADS; ++t) {
    int rc = pthread_join(threads[t], &sum1);
    if (rc) {
      cerr << "ERROR; return code from pthread_join() is " << rc << endl;
      exit(1);
    }
    cout << "Main: completed join with thread " << t << " having a result of "
         << * (int64_t*) sum1 << endl;
    sum += * (int64_t*) sum1;
  }
  cout << "max: " << max << endl;
  cout << "sum: " << sum << endl;
  return 0;
}
