#include "timer.hpp"
#include "interrupts.hpp"

template<console model>
void timer<model>::tick()
{
    //@formatter:off
    switch (current) {
        case tima_overflow:
            tima = tma;
            interrupts.req(interrupts::timer);
            current = reloading;
            break;
        case reloading: current = running; break;
        default: break;
    }
    //@formatter:on

    const bool old_mux{mux_output()};
    sysclk += 4;

    if (enabled and is_falling_edge(old_mux)) {
        increment_tima();
    }
}

template<console model>
void timer<model>::set_div()
{
    const bool old_mux{mux_output()};
    sysclk = 0;

    if (enabled and is_falling_edge(old_mux)) {
        increment_tima();
    }
}

template<console model>
void timer<model>::set_tima(u8 tima)
{
    if (current == running) {
        this->tima = tima;
    }
    else if (current == tima_overflow) {
        // writing to tima on cycle a acts as if the overflow didnt happen
        current = running;
        this->tima = tima;
    } // else writing to tima on cycle b is ignored
}

template<console model>
void timer<model>::set_tma(u8 tma)
{
    this->tma = tma;

    if (current == reloading) [[unlikely]] {
        tima = tma;
    }
}

template<console model>
void timer<model>::set_tac(u8 tac)
{
    const bool old_mux{mux_output()};
    sysclk_bit = tac_clock_bits[tac & 0b11];

    bool dmg_quirk{false};
    if constexpr (model == console::dmg) {
        dmg_quirk = enabled and not(tac & 0b100) and old_mux;
    }

    if (enabled and is_falling_edge(old_mux) or dmg_quirk) {
        increment_tima();
    }

    enabled = tac & 0b100;
    this->tac = tac;
}

template<console model>
void timer<model>::increment_tima()
{
    if (tima == 0xFF) {
        current = tima_overflow;
    }
    ++tima;
}

template<console model>
bool timer<model>::mux_output() const
{
    return (sysclk >> sysclk_bit) & 0b1;
}

template<console model>
bool timer<model>::is_falling_edge(const bool was_high) const
{
    return was_high and not mux_output();
}

template class timer<console::dmg>;
template class timer<console::cgb>;
