#pragma once

#include "types.hpp"
#include "scheduler.hpp"
#include "cpu.hpp"
#include "mmu.hpp"
#include "timer.hpp"
#include "interrupts.hpp"

template<console model>
class gb {
public:
    void run(long long cycles = -1);

private:
    interrupts interrupts{};
    timer<model> timer{interrupts};
    scheduler<model> scheduler{timer};
    mmu<model> mmu{scheduler, timer, interrupts};
    cpu<model> cpu{scheduler, mmu, interrupts};
};
