#pragma once

#include <fstream>
#include <iostream>

#include "types.hpp"

class memory {
public:
    memory()
    {
        std::ifstream file {"./roms/cpu_instrs.gb", std::ios::binary};

        if (!file.is_open()) {
            std::cerr << "Unable to open file" << std::endl;
            return;
        }

        file.read(reinterpret_cast<std::istream::char_type *>(&ram), 0xFFFF);

        if (file.bad()) {
            std::cerr << "Unable to read file" << std::endl;
            return;
        }
    }

    u8 read8(u16 addr) { return ram[addr]; }
    u16 read16(u16 addr) { return ram[static_cast<u16>(addr + 1)] << 8 | ram[addr]; }
    void write8(u16 addr, u8 data)
    {
        if (addr == 0xFF02 and data == 0x81) {
            std::cout << ram[0xFF01];
        }
        ram[addr] = data;
    }
    void write16(u16 addr, u16 data) { ram[addr] = data & 0xFF; ram[static_cast<u16>(addr + 1)] = data >> 8; }
private:
    u8 ram[0xFFFF] {};
};
