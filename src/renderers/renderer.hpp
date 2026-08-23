#pragma once

#include "../types.hpp"

class renderer {
public:
    virtual ~renderer() = default;

    virtual void render(const u32* buffer) = 0;
};
