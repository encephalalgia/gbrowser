#pragma once

#include "types.hpp"

class interrupts;

template<console model>
class timer {
public:
    explicit timer(interrupts& i)
        : interrupts{i}
    {}

    bool enabled{false};

    u8 tima{0x00};
    u8 tma{0xF8};
    u8 tac{0x00};

    void tick();

    [[nodiscard]] u8 get_div() const { return (sysclk >> 8) & 0xFF; }

    void set_div();

    void set_tima(u8 tima);

    void set_tma(u8 tma);

    void set_tac(u8 tac);

private:
    static constexpr u8 tac_clock_bits[4]{9, 3, 5, 7};

    enum state {
        running,
        tima_overflow,
        reloading,
    };

    interrupts& interrupts;
    state current{running};

    u16 sysclk{model == console::dmg ? 0xAB00 : 0x00};
    u8 sysclk_bit{0};

    void increment_tima();

    [[nodiscard]] bool mux_output() const;

    [[nodiscard]] bool is_falling_edge(bool was_high) const;
};
