#pragma once

#include "types.hpp"
#include <bit>

class interrupts {
public:
    enum source : u8 {
        vblank = 0b1,
        lcd = 0b10,
        timer = 0b100,
        serial = 0b1000,
        joypad = 0b10000,
    };

    u8 flag{0xE1};
    u8 enable{0x00};

    void req(u8 source);

    [[nodiscard]] u8 pending() const;

    [[nodiscard]] u16 consume();

private:
    static constexpr u16 base{0x40};
    static constexpr u16 scale{8};
};
