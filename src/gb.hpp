#pragma once

#include "types.hpp"
#include "scheduler.hpp"
#include "cpu.hpp"
#include "mmu.hpp"
#include "timer.hpp"
#include "interrupts.hpp"
#include "renderers/renderer.hpp"
#include "ppu.hpp"

template<console model>
class gb {
public:
    explicit gb(renderer& r)
        : renderer{r}
    {}

    void run(long long cycles = -1);

private:
    interrupts interrupts{};
    timer<model> timer{interrupts};
    renderer& renderer;
    ppu<model> ppu{renderer, interrupts};
    scheduler<model> scheduler{timer, ppu};
    mmu<model> mmu{scheduler, timer, ppu, interrupts};
    cpu<model> cpu{scheduler, mmu, interrupts};
};
