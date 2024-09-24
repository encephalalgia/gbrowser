cmake_minimum_required(VERSION 3.29)
project(gbrowser)

set(CMAKE_CXX_STANDARD 23)

add_executable(gbrowser main.cpp
        src/cpu.cpp
        src/cpu.hpp
        src/types.hpp)
