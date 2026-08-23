#include "src/types.hpp"
#include "src/gb.hpp"
#include "src/renderers/sdl_renderer.hpp"

using renderer_t = sdl_renderer;

int main()
{
    renderer_t renderer;
    gb<console::dmg> dmg{renderer};
    dmg.run(1e6);

    return 0;
}
