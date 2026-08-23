#include <fstream>
#include <iostream>
#include <filesystem>
#include "mmu.hpp"
#include "scheduler.hpp"
#include "timer.hpp"
#include "ppu.hpp"
#include "interrupts.hpp"

template<console model>
void mmu<model>::load_rom(const std::filesystem::path& path)
{
    std::ifstream file{path, std::ios::binary};

    if (!file.is_open()) {
        std::cerr << "Unable to open file" << std::endl;
        return;
    }

    file.read(reinterpret_cast<std::istream::char_type *>(&ram), 0xFFFF);

    if (file.bad()) {
        std::cerr << "Unable to read file" << std::endl;
    }
}

template<console model>
u8 mmu<model>::read8(const u16 addr)
{
    scheduler.tick();

    if (addr < 0x8000) {
        return rom[addr];
    }
    if (addr < 0xA000) {
        return ppu.read_vram(addr - 0x8000);
    }
    else if (addr < 0xE000) {
        return ram[addr - 0xA000];
    }
    else if (addr < 0xFE00) {
        return ram[addr - 0xC000];
    }
    else if (addr < 0xFEA0) {
        return ppu.read_oam(addr - 0xFE00);
    }
    else if (addr < 0xFF00) {
        if (ppu.is_oam_accessible()) {
            if constexpr (model == console::dmg) {
                return 0x00;
            }
            else {
                return ((addr & 0x00F0) >> 4) | (addr & 0x00F0);
            }
        }

        if constexpr (model == console::dmg) {
            ppu.corrupt_oam_rd();
        }

        return 0xFF;
    }
    else if (addr >= 0xFF80 and addr < 0xFFFF) {
        return hram[addr - 0xFF80];
    }

    return io_read(addr & 0xFF);
}

template<console model>
u16 mmu<model>::read16(const u16 addr)
{
    const u8 lo{read8(addr)};
    const u8 hi{read8(addr + 1)};
    return (hi << 8) | lo;
}

template<console model>
void mmu<model>::write8(const u16 addr, const u8 data)
{
    scheduler.tick();

    if (addr < 0x8000) {
        // attempt to write to rom
    }
    else if (addr < 0xA000) {
        ppu.write_vram(addr - 0x8000, data);
    }
    else if (addr < 0xE000) {
        ram[addr - 0xA000] = data;
    }
    else if (addr < 0xFE00) {
        ram[addr - 0xC000] = data;
    }
    else if (addr < 0xFEA0) {
        ppu.write_oam(addr - 0xFE00, data);
    }
    else if (addr < 0xFF00) {
        if constexpr (model == console::dmg) {
            if (not ppu.is_oam_accessible()) {
                ppu.corrupt_oam_wr();
            }
        }
    }
    else if (addr >= 0xFF80 and addr < 0xFFFF) {
        hram[addr - 0xFF80] = data;
    }
    else {
        io_write(addr & 0xFF, data);
    }
}

template<console model>
void mmu<model>::write16(const u16 addr, const u16 data)
{
    write8(addr, data & 0xFF);
    write8(addr + 1, data >> 8);
}

template<console model>
u8 mmu<model>::io_read(const u16 addr) const
{
    switch (addr) {
        case 0x02: return 0xFF;
        case div: return timer.get_div();
        case tima: return timer.tima;
        case tma: return timer.tma;
        case tac: return timer.tac;
        case lcdc: return ppu.lcdc;
        case stat: return ppu.stat;
        case scy: return ppu.scy;
        case scx: return ppu.scx;
        case ly: return ppu.ly;
        case lyc: return ppu.lyc;
        case dma: return ppu.dma;
        case bgp: return ppu.bgp;
        case obp0: return ppu.obp0;
        case obp1: return ppu.obp1;
        case wy: return ppu.wy;
        case wx: return ppu.wx;
        case interrupt_flag: return interrupts.flag;
        case interrupt_enable: return interrupts.enable;
        default: return unused_mmio_reg;
    }
}

template<console model>
void mmu<model>::io_write(const u16 addr, const u8 data)
{
    //@formatter:off
    switch (addr) {
        case 0x01: mmio[0x01] = data; break;
        case 0x02:
            mmio[0x02] = data;
            if (data & 0x80) {
                std::cout << static_cast<char>(mmio[0x01]) << std::flush;
                mmio[0x02] &= ~0x80;
            }
            break;
        case div: timer.set_div(); break;
        case tima: timer.set_tima(data); break;
        case tma: timer.set_tma(data); break;
        case tac: timer.set_tac(data); break;
        case lcdc: ppu.lcdc = data; break;
        case stat: ppu.stat = 0x80 | (data & 0x78) | (ppu.stat & 0x07); break;
        case scy: ppu.scy = data; break;
        case scx: ppu.scx = data; break;
        case ly: break;
        case lyc: ppu.lyc = data; break;
        case dma: ppu.set_dma(data); break;
        case bgp: ppu.bgp = data; break;
        case obp0: ppu.obp0 = data; break;
        case obp1: ppu.obp1 = data; break;
        case wy: ppu.wy = data; break;
        case wx: ppu.wx = data; break;
        case interrupt_flag: interrupts.flag = data | 0xE0; break;
        case interrupt_enable: interrupts.enable = data & 0x1F; break;
        default: break;
    }
    //@formatter:on
}

template<console model>
void mmu<model>::idu_input_rd(const u16 addr)
{
    if constexpr (model == console::dmg) {
        if (addr >= 0xFE00 and addr < 0xFF00) {
            ppu.corrupt_oam_rd();
        }
    }
}

template<console model>
void mmu<model>::idu_input_wr(const u16 addr)
{
    if constexpr (model == console::dmg) {
        if (addr >= 0xFE00 and addr < 0xFF00) {
            ppu.corrupt_oam_wr();
        }
    }
}

template<console model>
void mmu<model>::idu_input_rw(const u16 addr)
{
    if constexpr (model == console::dmg) {
        if (addr >= 0xFE00 and addr < 0xFF00) {
            ppu.corrupt_oam_rw();
        }
    }
}

template class mmu<console::dmg>;
template class mmu<console::cgb>;
