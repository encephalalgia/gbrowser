#pragma once

#include "../types.hpp"
#include "renderer.hpp"

class sdl_renderer : public renderer {
public:
    void render(const u32* buffer) override;

private:
};

