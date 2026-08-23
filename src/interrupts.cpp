#include "interrupts.hpp"
#include <bit>

void interrupts::req(const u8 source)
{
    flag |= source;
}

u8 interrupts::pending() const
{
    return flag & enable;
}

u16 interrupts::consume()
{
    const u8 queued{pending()};

    if (queued == 0) [[unlikely]] {
        return 0;
    }

    const int index{std::countr_zero(queued)};

    flag ^= (1U << index);
    return base + (index * scale);
}
