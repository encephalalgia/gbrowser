#include "scheduler.hpp"
#include "timer.hpp"

template<console model>
void scheduler<model>::tick() const
{
    timer.tick();
    ppu.tick(4);
}

template<console model>
void scheduler<model>::stop()
{
    stopped = true;
    timer.set_div();
}

template<console model>
void scheduler<model>::wake()
{
    stopped = false;
}

template class scheduler<console::dmg>;
template class scheduler<console::cgb>;
