#pragma once

#include "types.hpp"

template<console model>
class timer;

template<console model>
class scheduler {
public:
    explicit scheduler(timer<model>& t)
        : timer{t}
    {}

    bool stopped{false};

    void tick() const;

    void stop();

    void wake();

private:
    timer<model>& timer;
    u32 cycle{};
};
