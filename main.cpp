#include "src/types.hpp"
#include "src/gb.hpp"

int main()
{
    gb<console::dmg> dmg{};
    dmg.run(1e6);

    return 0;
}
