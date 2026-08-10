cmake_minimum_required(VERSION 3.29)
project(gbrowser)

set(CMAKE_CXX_STANDARD 23)

add_executable(gbrowser main.cpp
        src/types.hpp
        src/gb.cpp
        src/gb.hpp
        src/scheduler.cpp
        src/scheduler.hpp
        src/cpu.cpp
        src/cpu.hpp
        src/mmu.cpp
        src/mmu.hpp
        src/timer.cpp
        src/timer.hpp
        src/interrupts.hpp
        src/interrupts.cpp
)

include(CheckIPOSupported)
check_ipo_supported(RESULT ipo_supported OUTPUT ipo_error)
if (ipo_supported)
    set_property(TARGET gbrowser PROPERTY INTERPROCEDURAL_OPTIMIZATION_RELEASE TRUE)
else ()
    message(WARNING "IPO is not supported: ${output}")
endif ()
