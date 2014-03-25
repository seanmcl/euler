cmake_minimum_required(VERSION 2.8)
set(CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libc++")
project(001)

include_directories(${GMP_INCLUDE_DIR})

add_executable(001 main.cc)
target_link_libraries(001 gmp gmpxx)
