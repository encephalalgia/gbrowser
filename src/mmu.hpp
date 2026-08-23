#pragma once

#include <filesystem>

#include "types.hpp"

template<console model>
class scheduler;

template<console model>
class timer;

template<console model>
class ppu;

class interrupts;

template<console model>
class mmu {
public:
    mmu(scheduler<model>& s, timer<model>& t, ppu<model>& p, interrupts& i)
        : scheduler{s}, timer{t}, ppu{p}, interrupts{i}
    {
        load_rom("./roms/instr_timing.gb");
    }

    enum io_register : unsigned {
        // Timer
        div = 0x04,
        tima = 0x05,
        tma = 0x06,
        tac = 0x07,
        // Graphics
        lcdc = 0x40,
        stat = 0x41,
        scy = 0x42,
        scx = 0x43,
        ly = 0x44,
        lyc = 0x45,
        dma = 0x46,
        bgp = 0x47,
        obp0 = 0x48,
        obp1 = 0x49,
        wy = 0x4A,
        wx = 0x4B,
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

    void io_write(u16 addr, u8 data);

    void idu_input_rd(u16 addr);

    void idu_input_wr(u16 addr);

    void idu_input_rw(u16 addr);

    u8 rom[0x8000]{};
    u8 ram[0x4000]{};
    u8 hram[0x7F]{};

private:
    static constexpr u8 unused_mmio_reg{0xFF};

    scheduler<model>& scheduler;
    timer<model>& timer;
    ppu<model>& ppu;
    interrupts& interrupts;
};
