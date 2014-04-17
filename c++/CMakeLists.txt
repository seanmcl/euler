cmake_minimum_required(VERSION 2.8)
set(CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libc++")
# set(CMAKE_BUILD_TYPE Release)
project(Euler)

# find_package (Threads)
# target_link_libraries (0001_threads ${CMAKE_THREAD_LIBS_INIT})

# include_directories(${GMP_INCLUDE_DIR})
ADD_DEFINITIONS(-DBOOST_LOG_DYN_LINK)

add_library(util util.cc)

### 0001

add_executable(0001 0001.cc)

add_executable(0001_gmp 0001_gmp.cc)
target_link_libraries(0001_gmp gmp gmpxx)

add_executable(0001_threads 0001_threads.cc)
target_link_libraries(0001_threads boost_log-mt pthread)

### 0002

add_executable(0002 0002.cc)

### 0003

add_executable(0003 0003.cc)
target_link_libraries(0003 boost_log-mt util)
