#pragma once

#include <filesystem>

#include "types.hpp"

template<console model>
class scheduler;

template<console model>
class timer;

class interrupts;

template<console model>
class mmu {
public:
    mmu(scheduler<model>& s, timer<model>& t, interrupts& i)
        : scheduler{s}, timer{t}, interrupts{i}
    {
        load_rom("./roms/instr_timing.gb");
    }

    enum io_register : unsigned {
        // Timer
        div = 0x04,
        tima = 0x05,
        tma = 0x06,
        tac = 0x07,
        // Interrupts
        interrupt_flag = 0x0F,
        interrupt_enable = 0xFF,
    };

    void load_rom(const std::filesystem::path& path);

    [[nodiscard]] u8 read8(u16 addr);

    void write8(u16 addr, u8 data);

    [[nodiscard]] u16 read16(u16 addr);

    void write16(u16 addr, u16 data);

    [[nodiscard]] u8 io_read(u16 addr) const;

    void io_write(u16 addr, u8 val);

    u8 ram[0xFFFF]{};

private:
    scheduler<model>& scheduler;
    timer<model>& timer;
    interrupts& interrupts;

    u8 mmio[0x100]{
        model == console::dmg ? 0xCF : 0xC7, 0x00, model == console::dmg ? 0x7E : 0x7F, 0x00, 0x00, 0x00, 0xF8, 0x00,
        0x00, 0x00, 0x80, 0xBF, 0xF3, 0xFF, 0xBF, 0x00,
        0x3F, 0x00, 0xFF, 0xBF, 0x00, 0x00, 0x00, 0xBF, 0x77, 0xF3, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x91, 0x85, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFC, // 0x00 for the remaining values
    };
};
