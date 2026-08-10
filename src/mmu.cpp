#include <fstream>
#include <iostream>
#include <filesystem>
#include "mmu.hpp"
#include "scheduler.hpp"
#include "timer.hpp"
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

    if (addr < 0xFF00) {
        return ram[addr];
    }
    return io_read(addr & 0xFF);
}

template<console model>
u16 mmu<model>::read16(const u16 addr)
{
    const u8 lo = read8(addr);
    const u8 hi = read8(addr + 1);
    return (static_cast<u16>(hi) << 8) | lo;
}

template<console model>
void mmu<model>::write8(const u16 addr, const u8 data)
{
    scheduler.tick();

    if (addr < 0xFF00 and addr > 0x7FFF) {
        ram[addr] = data;
    }
    else if (addr == 0xFF01) {
        mmio[0x01] = data;
    }
    else if (addr == 0xFF02) {
        mmio[0x02] = data;

        if (data & 0x80) {
            std::cout << static_cast<char>(mmio[0x01]) << std::flush;
            mmio[0x02] &= ~0x80;
        }
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
        case 0x02:
        case 0x44: return 0xFF;
        case div: return timer.get_div();
        case tima: return timer.tima;
        case tma: return timer.tma;
        case tac: return timer.tac;
        case interrupt_flag: return interrupts.flag;
        case interrupt_enable: return interrupts.enable;
        default: return mmio[addr];
    }
}

template<console model>
void mmu<model>::io_write(const u16 addr, const u8 val)
{
    //@formatter:off
    switch (addr) {
        case div: timer.set_div(); break;
        case tima: timer.set_tima(val); break;
        case tma: timer.set_tma(val); break;
        case tac: timer.set_tac(val); break;
        case interrupt_flag: interrupts.flag = val | 0xE0; break;
        case interrupt_enable: interrupts.enable = val & 0x1F; break;
        default: mmio[addr] = val; break;
    }
    //@formatter:on
}

template class mmu<console::dmg>;
template class mmu<console::cgb>;
