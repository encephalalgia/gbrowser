#include <iostream>

#include "src/cpu.hpp"
#include "src/memory.hpp"

constexpr bool log {false};
constexpr bool step_debug {false};

int main()
{
    memory memory {};
    cpu cpu {memory};

    char step {};
    for (int i {0}; i != 1e6; ++i) {
        if (log) {
#ifndef NDEBUG
            std::cout << cpu.to_string() << '\n';
#endif
        }
        cpu.tick();
        if (log and step_debug) {
            std::cin.ignore();
        }
    }
    return 0;
}
