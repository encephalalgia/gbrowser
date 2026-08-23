#pragma once

#include "ppu.hpp"
#include "types.hpp"

template<console model>
class timer;

template<console model>
class ppu;

template<console model>
class scheduler {
public:
    scheduler(timer<model>& t, ppu<model>& p)
        : timer{t}, ppu{p}
    {}

    bool stopped{false};

    void tick() const;

    void stop();

    void wake();

private:
    timer<model>& timer;
    ppu<model>& ppu;
    u32 cycle{};
};
