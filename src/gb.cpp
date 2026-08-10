#include "gb.hpp"

#ifndef NDEBUG
#include <iostream>
constexpr bool log{false};
constexpr bool step_debug{false};
#endif

template<console model>
void gb<model>::run(const long long cycles)
{
    if (cycles < -1) {
    }
    else {
        while (true) {
#ifndef NDEBUG
            if constexpr (log) {
                std::cout << cpu.to_string() << '\n';
            }
#endif
            if (not scheduler.stopped) {
                cpu.tick();
            }
#ifndef NDEBUG
            if constexpr (log and step_debug) {
                std::cin.ignore();
            }
#endif
        }
    }
}

template class gb<console::dmg>;
template class gb<console::cgb>;
