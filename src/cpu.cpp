#include "cpu.hpp"
#include <utility>
#include "scheduler.hpp"
#include "mmu.hpp"
#include "interrupts.hpp"

template<console model>
template<auto M>
consteval bool cpu<model>::is_8_bit()
{
    return M != address_mode::bc
           and M != address_mode::de
           and M != address_mode::hl
           and M != address_mode::af
           and M != address_mode::sp
           and M != address_mode::imm_16;
}

template<console model>
template<auto cc>
bool cpu<model>::evaluate_condition() const
{
    switch (cc) {
        case condition::none: return true;
        case condition::nz: return !zf;
        case condition::z: return zf;
        case condition::nc: return !cf;
        case condition::c: return cf;
    }

    std::unreachable();
}

template<console model>
template<auto hi, auto lo>
u16 cpu<model>::get_pair() const
{
    if constexpr (lo == f) {
        return static_cast<u16>(reg[hi]) << 8 | zf << zbit | nf << nbit | hf << hbit | cf << cbit;
    }
    else {
        return static_cast<u16>(reg[hi]) << 8 | reg[lo];
    }
}

template<console model>
template<auto hi, auto lo>
void cpu<model>::set_pair(const u16 val)
{
    reg[hi] = val >> 8;

    if constexpr (lo == f) {
        zf = (val >> zbit) & 1;
        nf = (val >> nbit) & 1;
        hf = (val >> hbit) & 1;
        cf = (val >> cbit) & 1;
    }
    else {
        reg[lo] = val & 0xFF;
    }
}

template<console model>
template<auto M>
cpu<model>::template operand_t<M> cpu<model>::get_operand()
{
    if constexpr (M == address_mode::b) return reg[b];
    else if constexpr (M == address_mode::c) return reg[c];
    else if constexpr (M == address_mode::d) return reg[d];
    else if constexpr (M == address_mode::e) return reg[e];
    else if constexpr (M == address_mode::h) return reg[h];
    else if constexpr (M == address_mode::l) return reg[l];
    else if constexpr (M == address_mode::a) return reg[a];
    else if constexpr (M == address_mode::bc) return get_pair<b, c>();
    else if constexpr (M == address_mode::de) return get_pair<d, e>();
    else if constexpr (M == address_mode::hl) return get_pair<h, l>();
    else if constexpr (M == address_mode::af) return get_pair<a, f>();
    else if constexpr (M == address_mode::sp) return sp;
    else if constexpr (M == address_mode::bc_indirect) return mmu.read8(get_pair<b, c>());
    else if constexpr (M == address_mode::de_indirect) return mmu.read8(get_pair<d, e>());
    else if constexpr (M == address_mode::hl_indirect) return mmu.read8(get_pair<h, l>());
    else if constexpr (M == address_mode::hl_inc) {
        const u16 tmp{get_pair<h, l>()};
        set_pair<h, l>(tmp + 1);
        return mmu.read8(tmp);
    }
    else if constexpr (M == address_mode::hl_dec) {
        const u16 tmp{get_pair<h, l>()};
        set_pair<h, l>(tmp - 1);
        return mmu.read8(tmp);
    }
    else if constexpr (M == address_mode::imm_8) return mmu.read8(pc++);
    else if constexpr (M == address_mode::imm_16) {
        pc += 2;
        return mmu.read16(pc - 2);
    }
    else if constexpr (M == address_mode::imm_16_indirect) return mmu.read8(get_operand<address_mode::imm_16>());

    std::unreachable();
}

template<console model>
template<auto M>
void cpu<model>::set_operand(operand_t<M> data)
{
    static_assert(
        M != address_mode::imm_8 and M != address_mode::imm_16,
        "can't write to an immediate type"
    );

    if constexpr (M == address_mode::b) reg[b] = data;
    else if constexpr (M == address_mode::c) reg[c] = data;
    else if constexpr (M == address_mode::d) reg[d] = data;
    else if constexpr (M == address_mode::e) reg[e] = data;
    else if constexpr (M == address_mode::h) reg[h] = data;
    else if constexpr (M == address_mode::l) reg[l] = data;
    else if constexpr (M == address_mode::a) reg[a] = data;
    else if constexpr (M == address_mode::bc) set_pair<b, c>(data);
    else if constexpr (M == address_mode::de) set_pair<d, e>(data);
    else if constexpr (M == address_mode::hl) set_pair<h, l>(data);
    else if constexpr (M == address_mode::af) set_pair<a, f>(data);
    else if constexpr (M == address_mode::sp) sp = data;
    else if constexpr (M == address_mode::bc_indirect) mmu.write8(get_pair<b, c>(), data);
    else if constexpr (M == address_mode::de_indirect) mmu.write8(get_pair<d, e>(), data);
    else if constexpr (M == address_mode::hl_indirect) mmu.write8(get_pair<h, l>(), data);
    else if constexpr (M == address_mode::hl_inc) {
        const u16 tmp{get_pair<h, l>()};
        set_pair<h, l>(tmp + 1);
        mmu.write8(tmp, data);
    }
    else if constexpr (M == address_mode::hl_dec) {
        const u16 tmp{get_pair<h, l>()};
        set_pair<h, l>(tmp - 1);
        mmu.write8(tmp, data);
    }
    else if constexpr (M == address_mode::imm_16_indirect) mmu.write8(get_operand<address_mode::imm_16>(), data);
}

template<console model>
void cpu<model>::service_interrupt()
{
    ime = 0;

    if (halt_bugged) [[unlikely]] {
        halt_bugged = false;
        --pc;
    }

    scheduler.tick();
    scheduler.tick();

    mmu.write8(--sp, pc >> 8);
    const u16 handler{interrupts.consume()};
    mmu.write8(--sp, pc & 0xFF);

    pc = handler;
    scheduler.tick();
}

template<console model>
u8 cpu<model>::fetch()
{
    const u8 opcode{mmu.read8(pc)};

    if (halt_bugged) [[unlikely]] {
        halt_bugged = false;
    }
    else [[likely]] {
        ++pc;
    }

    return opcode;
}

#pragma region Misc Instructions
template<console model>
void cpu<model>::stop()
{
    ++pc; // STOP skips one byte after itself
    scheduler.stop();
}

template<console model>
void cpu<model>::halt()
{
    if (ime != 1 and interrupts.pending()) [[unlikely]] {
        halt_bugged = true;
    }
    else [[likely]] {
        halted = true;
    }
}

template<console model>
void cpu<model>::di()
{
    ime = 0;
}

template<console model>
void cpu<model>::ei()
{
    if (ime != 1) {
        ime = 2;
    }
}
#pragma endregion

#pragma region Load Instructions
template<console model>
template<auto dst, auto src>
void cpu<model>::ld()
{
    set_operand<dst>(get_operand<src>());
}

template<console model>
template<auto dst, auto src>
void cpu<model>::ldh()
{
    const auto addr{0xFF00 + get_operand<dst == address_mode::a ? src : dst>()};

    if constexpr (dst == address_mode::a) {
        reg[a] = mmu.read8(addr);
    }
    else {
        mmu.write8(addr, reg[a]);
    }
}

template<console model>
template<auto M>
void cpu<model>::ld_sp_e()
{
    const u8 e{get_operand<address_mode::imm_8>()};
    const s8 d{static_cast<s8>(e)};
    const u16 sum{static_cast<u16>(sp + d)};

    zf = nf = false;
    hf = ((sp & 0xF) + (e & 0xF)) > 0xF;
    cf = ((sp & 0xFF) + e) > 0xFF;

    scheduler.tick();
    if constexpr (M == address_mode::sp) {
        scheduler.tick();
    }
    set_operand<M>(sum);
}

template<console model>
void cpu<model>::ld_sp_hl()
{
    scheduler.tick();
    sp = get_operand<address_mode::hl>();
}

template<console model>
void cpu<model>::ld_nn_sp()
{
    mmu.write16(get_operand<address_mode::imm_16>(), sp);
}

template<console model>
template<auto M>
void cpu<model>::pop()
{
    set_operand<M>(mmu.read16(sp));
    sp += 2;
}

template<console model>
template<auto M>
void cpu<model>::push()
{
    const u16 data{get_operand<M>()};
    scheduler.tick();
    mmu.write8(--sp, data >> 8);
    mmu.write8(--sp, data & 0xFF);
}
#pragma endregion

#pragma region ALU Instructions
template<console model>
template<auto M>
void cpu<model>::add()
{
    if constexpr (is_8_bit<M>()) {
        do_arithmetic<true>(get_operand<M>());
    }
    else {
        const u16 augend{get_operand<address_mode::hl>()};
        const u16 addend{get_operand<M>()};
        const u32 sum{static_cast<u32>(augend) + addend};

        nf = false;
        hf = (augend ^ addend ^ sum) & 0x1000;
        cf = sum > 0xFFFF;

        scheduler.tick();
        set_operand<address_mode::hl>(sum);
    }
}

template<console model>
template<auto M>
void cpu<model>::adc()
{
    do_arithmetic<true>(get_operand<M>(), cf);
}

template<console model>
template<auto M>
void cpu<model>::sub()
{
    do_arithmetic<false>(get_operand<M>());
}

template<console model>
template<auto M>
void cpu<model>::sbc()
{
    do_arithmetic<false>(get_operand<M>(), cf);
}

template<console model>
template<auto M>
void cpu<model>::land()
{
    reg[a] &= get_operand<M>();
    set_logic_flags<true>();
}

template<console model>
template<auto M>
void cpu<model>::lxor()
{
    reg[a] ^= get_operand<M>();
    set_logic_flags<false>();
}

template<console model>
template<auto M>
void cpu<model>::lor()
{
    reg[a] |= get_operand<M>();
    set_logic_flags<false>();
}

template<console model>
template<auto M>
void cpu<model>::cp()
{
    const u8 tmp{reg[a]};
    do_arithmetic<false>(get_operand<M>());
    reg[a] = tmp;
}

template<console model>
template<auto M>
void cpu<model>::inc()
{
    do_increment<M, 1>();
}

template<console model>
template<auto M>
void cpu<model>::dec()
{
    do_increment<M, -1>();
}

template<console model>
template<bool was_and>
void cpu<model>::set_logic_flags()
{
    zf = reg[a] == 0;
    nf = cf = false;
    hf = was_and;
}

template<console model>
template<bool addition>
void cpu<model>::do_arithmetic(const u8 operand, const bool cy)
{
    u16 res;
    if constexpr (addition) {
        res = reg[a] + operand + cy;
        cf = res > 0xFF;
    }
    else {
        res = reg[a] - operand - cy;
        cf = reg[a] < static_cast<u16>(operand + cy);
    }

    zf = (res & 0xFF) == 0;
    nf = !addition;
    hf = (reg[a] ^ operand ^ res) & 0x10;
    reg[a] = res;
}

template<console model>
template<auto M, int sign>
void cpu<model>::do_increment()
{
    if constexpr (is_8_bit<M>()) {
        const u8 res{static_cast<u8>(get_operand<M>() + sign)};
        zf = res == 0;
        nf = sign < 0;
        if constexpr (sign == 1) {
            hf = (res & 0xF) == 0;
        }
        else {
            hf = (res & 0xF) == 0xF;
        }
        set_operand<M>(res);
    }
    else {
        scheduler.tick();
        set_operand<M>(get_operand<M>() + sign);
    }
}
#pragma endregion

#pragma region Rotate/Shift, Bit Instructions
template<console model>
template<auto M>
void cpu<model>::rlc()
{
    u8 val{get_operand<M>()};

    cf = val & 0x80;
    val = (val << 1) | cf;

    set_shift_flags(val);
    set_operand<M>(val);
}

template<console model>
template<auto M>
void cpu<model>::rrc()
{
    u8 val{get_operand<M>()};

    cf = val & 1;
    val = (val >> 1) | (cf << 7);

    set_shift_flags(val);
    set_operand<M>(val);
}

template<console model>
template<auto M>
void cpu<model>::rl()
{
    u8 val{get_operand<M>()};

    const bool old_cf{cf};
    cf = val & 0x80;
    val = (val << 1) | old_cf;

    set_shift_flags(val);
    set_operand<M>(val);
}

template<console model>
template<auto M>
void cpu<model>::rr()
{
    u8 val{get_operand<M>()};

    const bool old_cf{cf};
    cf = val & 1;
    val = (val >> 1) | (old_cf << 7);

    set_shift_flags(val);
    set_operand<M>(val);
}

template<console model>
template<auto M>
void cpu<model>::sla()
{
    u8 val{get_operand<M>()};

    cf = val & 0x80;
    val <<= 1;

    set_shift_flags(val);
    set_operand<M>(val);
}

template<console model>
template<auto M>
void cpu<model>::sra()
{
    u8 val{get_operand<M>()};

    const u8 old_b7{static_cast<u8>(val & 0x80)};
    cf = val & 1;
    val = (val >> 1) | old_b7;

    set_shift_flags(val);
    set_operand<M>(val);
}

template<console model>
template<auto M>
void cpu<model>::swap()
{
    u8 val{get_operand<M>()};

    cf = false;
    val = (val << 4) | (val >> 4);

    set_shift_flags(val);
    set_operand<M>(val);
}

template<console model>
template<auto M>
void cpu<model>::srl()
{
    u8 val{get_operand<M>()};

    cf = val & 1;
    val >>= 1;

    set_shift_flags(val);
    set_operand<M>(val);
}

template<console model>
template<auto M, u8 b3>
void cpu<model>::bit()
{
    const u8 val{get_operand<M>()};
    zf = ((val >> b3) & 1) == 0;
    nf = false;
    hf = true;
}

template<console model>
template<auto M, u8 b3>
void cpu<model>::res()
{
    set_operand<M>(get_operand<M>() & ~(1u << b3));
}

template<console model>
template<auto M, u8 b3>
void cpu<model>::set()
{
    set_operand<M>(get_operand<M>() | (1u << b3));
}

template<console model>
void cpu<model>::set_shift_flags(const u8 val)
{
    zf = val == 0;
    nf = hf = false;
}
#pragma endregion

#pragma region Acummulator/Flag Instructions
template<console model>
void cpu<model>::daa()
{
    if (nf) {
        if (cf) {
            reg[a] -= 0x60;
        }

        if (hf) {
            reg[a] -= 0x06;
        }
    }
    else {
        if (cf or reg[a] > 0x99) {
            reg[a] += 0x60;
            cf = true;
        }

        if (hf or (reg[a] & 0x0F) > 0x09) {
            reg[a] += 0x06;
        }
    }

    zf = reg[a] == 0;
    hf = false;
}

template<console model>
void cpu<model>::cpl()
{
    reg[a] = ~reg[a];
    nf = hf = true;
}

template<console model>
void cpu<model>::scf()
{
    nf = hf = false;
    cf = true;
}

template<console model>
void cpu<model>::ccf()
{
    nf = hf = false;
    cf = !cf;
}

template<console model>
void cpu<model>::rlca()
{
    cf = reg[a] & 0x80;
    reg[a] = (reg[a] << 1) | cf;
    zf = nf = hf = false;
}

template<console model>
void cpu<model>::rla()
{
    const bool old_cf{cf};
    cf = reg[a] & 0x80;
    reg[a] = (reg[a] << 1) | old_cf;
    zf = nf = hf = false;
}

template<console model>
void cpu<model>::rrca()
{
    cf = reg[a] & 1;
    reg[a] = (reg[a] >> 1) | (cf << 7);
    zf = nf = hf = false;
}

template<console model>
void cpu<model>::rra()
{
    const bool old_cf{cf};
    cf = reg[a] & 1;
    reg[a] = (reg[a] >> 1) | (old_cf << 7);
    zf = nf = hf = false;
}
#pragma endregion

#pragma region Branch Instructions
template<console model>
template<auto cc>
void cpu<model>::ret()
{
    if constexpr (cc != condition::none) scheduler.tick();
    if (not evaluate_condition<cc>()) return;

    const u16 tmp{mmu.read16(sp)};
    scheduler.tick();
    pc = tmp;
    sp += 2;
}

template<console model>
void cpu<model>::reti()
{
    ime = 1;
    ret();
}

template<console model>
template<auto cc>
void cpu<model>::jp()
{
    const u16 addr{get_operand<address_mode::imm_16>()};
    if (not evaluate_condition<cc>()) return;

    scheduler.tick();
    pc = addr;
}

template<console model>
template<auto cc>
void cpu<model>::jr()
{
    const s8 d{static_cast<s8>(get_operand<address_mode::imm_8>())};
    if (not evaluate_condition<cc>()) return;

    scheduler.tick();
    pc += d;
}

template<console model>
template<auto cc>
void cpu<model>::call()
{
    const u16 addr{get_operand<address_mode::imm_16>()};
    if (not evaluate_condition<cc>()) return;

    push_pc();
    pc = addr;
}

template<console model>
template<u8 tgt3>
void cpu<model>::rst()
{
    push_pc();
    pc = tgt3;
}

template<console model>
void cpu<model>::push_pc()
{
    scheduler.tick();
    mmu.write8(--sp, pc >> 8);
    mmu.write8(--sp, pc & 0xFF);
}
#pragma endregion

template<console model>
void cpu<model>::tick()
{
    if (interrupts.pending()) {
        halted = false;

        if (ime == 1) {
            service_interrupt();
            return;
        }
    }

    if (ime == 2) {
        ime = 1;
    }

    if (halted) {
        scheduler.tick();
        return;
    }

    //@formatter:off
    switch (fetch()) {
        case 0x00: break; // NOP
        case 0x01: ld<address_mode::bc, address_mode::imm_16>(); break;
        case 0x02: ld<address_mode::bc_indirect, address_mode::a>(); break;
        case 0x03: inc<address_mode::bc>(); break;
        case 0x04: inc<address_mode::b>(); break;
        case 0x05: dec<address_mode::b>(); break;
        case 0x06: ld<address_mode::b, address_mode::imm_8>(); break;
        case 0x07: rlca(); break;
        case 0x08: ld_nn_sp(); break;
        case 0x09: add<address_mode::bc>(); break;
        case 0x0A: ld<address_mode::a, address_mode::bc_indirect>(); break;
        case 0x0B: dec<address_mode::bc>(); break;
        case 0x0C: inc<address_mode::c>(); break;
        case 0x0D: dec<address_mode::c>(); break;
        case 0x0E: ld<address_mode::c, address_mode::imm_8>(); break;
        case 0x0F: rrca(); break;

        case 0x10: stop(); break;
        case 0x11: ld<address_mode::de, address_mode::imm_16>(); break;
        case 0x12: ld<address_mode::de_indirect, address_mode::a>(); break;
        case 0x13: inc<address_mode::de>(); break;
        case 0x14: inc<address_mode::d>(); break;
        case 0x15: dec<address_mode::d>(); break;
        case 0x16: ld<address_mode::d, address_mode::imm_8>(); break;
        case 0x17: rla(); break;
        case 0x18: jr(); break;
        case 0x19: add<address_mode::de>(); break;
        case 0x1A: ld<address_mode::a, address_mode::de_indirect>(); break;
        case 0x1B: dec<address_mode::de>(); break;
        case 0x1C: inc<address_mode::e>(); break;
        case 0x1D: dec<address_mode::e>(); break;
        case 0x1E: ld<address_mode::e, address_mode::imm_8>(); break;
        case 0x1F: rra(); break;

        case 0x20: jr<condition::nz>(); break;
        case 0x21: ld<address_mode::hl, address_mode::imm_16>(); break;
        case 0x22: ld<address_mode::hl_inc, address_mode::a>(); break;
        case 0x23: inc<address_mode::hl>(); break;
        case 0x24: inc<address_mode::h>(); break;
        case 0x25: dec<address_mode::h>(); break;
        case 0x26: ld<address_mode::h, address_mode::imm_8>(); break;
        case 0x27: daa(); break;
        case 0x28: jr<condition::z>(); break;
        case 0x29: add<address_mode::hl>(); break;
        case 0x2A: ld<address_mode::a, address_mode::hl_inc>(); break;
        case 0x2B: dec<address_mode::hl>(); break;
        case 0x2C: inc<address_mode::l>(); break;
        case 0x2D: dec<address_mode::l>(); break;
        case 0x2E: ld<address_mode::l, address_mode::imm_8>(); break;
        case 0x2F: cpl(); break;

        case 0x30: jr<condition::nc>(); break;
        case 0x31: ld<address_mode::sp, address_mode::imm_16>(); break;
        case 0x32: ld<address_mode::hl_dec, address_mode::a>(); break;
        case 0x33: inc<address_mode::sp>(); break;
        case 0x34: inc<address_mode::hl_indirect>(); break;
        case 0x35: dec<address_mode::hl_indirect>(); break;
        case 0x36: ld<address_mode::hl_indirect, address_mode::imm_8>(); break;
        case 0x37: scf(); break;
        case 0x38: jr<condition::c>(); break;
        case 0x39: add<address_mode::sp>(); break;
        case 0x3A: ld<address_mode::a, address_mode::hl_dec>(); break;
        case 0x3B: dec<address_mode::sp>(); break;
        case 0x3C: inc<address_mode::a>(); break;
        case 0x3D: dec<address_mode::a>(); break;
        case 0x3E: ld<address_mode::a, address_mode::imm_8>(); break;
        case 0x3F: ccf(); break;

        case 0x40: ld<address_mode::b, address_mode::b>(); break;
        case 0x41: ld<address_mode::b, address_mode::c>(); break;
        case 0x42: ld<address_mode::b, address_mode::d>(); break;
        case 0x43: ld<address_mode::b, address_mode::e>(); break;
        case 0x44: ld<address_mode::b, address_mode::h>(); break;
        case 0x45: ld<address_mode::b, address_mode::l>(); break;
        case 0x46: ld<address_mode::b, address_mode::hl_indirect>(); break;
        case 0x47: ld<address_mode::b, address_mode::a>(); break;
        case 0x48: ld<address_mode::c, address_mode::b>(); break;
        case 0x49: ld<address_mode::c, address_mode::c>(); break;
        case 0x4A: ld<address_mode::c, address_mode::d>(); break;
        case 0x4B: ld<address_mode::c, address_mode::e>(); break;
        case 0x4C: ld<address_mode::c, address_mode::h>(); break;
        case 0x4D: ld<address_mode::c, address_mode::l>(); break;
        case 0x4E: ld<address_mode::c, address_mode::hl_indirect>(); break;
        case 0x4F: ld<address_mode::c, address_mode::a>(); break;

        case 0x50: ld<address_mode::d, address_mode::b>(); break;
        case 0x51: ld<address_mode::d, address_mode::c>(); break;
        case 0x52: ld<address_mode::d, address_mode::d>(); break;
        case 0x53: ld<address_mode::d, address_mode::e>(); break;
        case 0x54: ld<address_mode::d, address_mode::h>(); break;
        case 0x55: ld<address_mode::d, address_mode::l>(); break;
        case 0x56: ld<address_mode::d, address_mode::hl_indirect>(); break;
        case 0x57: ld<address_mode::d, address_mode::a>(); break;
        case 0x58: ld<address_mode::e, address_mode::b>(); break;
        case 0x59: ld<address_mode::e, address_mode::c>(); break;
        case 0x5A: ld<address_mode::e, address_mode::d>(); break;
        case 0x5B: ld<address_mode::e, address_mode::e>(); break;
        case 0x5C: ld<address_mode::e, address_mode::h>(); break;
        case 0x5D: ld<address_mode::e, address_mode::l>(); break;
        case 0x5E: ld<address_mode::e, address_mode::hl_indirect>(); break;
        case 0x5F: ld<address_mode::e, address_mode::a>(); break;

        case 0x60: ld<address_mode::h, address_mode::b>(); break;
        case 0x61: ld<address_mode::h, address_mode::c>(); break;
        case 0x62: ld<address_mode::h, address_mode::d>(); break;
        case 0x63: ld<address_mode::h, address_mode::e>(); break;
        case 0x64: ld<address_mode::h, address_mode::h>(); break;
        case 0x65: ld<address_mode::h, address_mode::l>(); break;
        case 0x66: ld<address_mode::h, address_mode::hl_indirect>(); break;
        case 0x67: ld<address_mode::h, address_mode::a>(); break;
        case 0x68: ld<address_mode::l, address_mode::b>(); break;
        case 0x69: ld<address_mode::l, address_mode::c>(); break;
        case 0x6A: ld<address_mode::l, address_mode::d>(); break;
        case 0x6B: ld<address_mode::l, address_mode::e>(); break;
        case 0x6C: ld<address_mode::l, address_mode::h>(); break;
        case 0x6D: ld<address_mode::l, address_mode::l>(); break;
        case 0x6E: ld<address_mode::l, address_mode::hl_indirect>(); break;
        case 0x6F: ld<address_mode::l, address_mode::a>(); break;

        case 0x70: ld<address_mode::hl_indirect, address_mode::b>(); break;
        case 0x71: ld<address_mode::hl_indirect, address_mode::c>(); break;
        case 0x72: ld<address_mode::hl_indirect, address_mode::d>(); break;
        case 0x73: ld<address_mode::hl_indirect, address_mode::e>(); break;
        case 0x74: ld<address_mode::hl_indirect, address_mode::h>(); break;
        case 0x75: ld<address_mode::hl_indirect, address_mode::l>(); break;
        case 0x76: halt(); break;
        case 0x77: ld<address_mode::hl_indirect, address_mode::a>(); break;
        case 0x78: ld<address_mode::a, address_mode::b>(); break;
        case 0x79: ld<address_mode::a, address_mode::c>(); break;
        case 0x7A: ld<address_mode::a, address_mode::d>(); break;
        case 0x7B: ld<address_mode::a, address_mode::e>(); break;
        case 0x7C: ld<address_mode::a, address_mode::h>(); break;
        case 0x7D: ld<address_mode::a, address_mode::l>(); break;
        case 0x7E: ld<address_mode::a, address_mode::hl_indirect>(); break;
        case 0x7F: ld<address_mode::a, address_mode::a>(); break;

        case 0x80: add<address_mode::b>(); break;
        case 0x81: add<address_mode::c>(); break;
        case 0x82: add<address_mode::d>(); break;
        case 0x83: add<address_mode::e>(); break;
        case 0x84: add<address_mode::h>(); break;
        case 0x85: add<address_mode::l>(); break;
        case 0x86: add<address_mode::hl_indirect>(); break;
        case 0x87: add<address_mode::a>(); break;
        case 0x88: adc<address_mode::b>(); break;
        case 0x89: adc<address_mode::c>(); break;
        case 0x8A: adc<address_mode::d>(); break;
        case 0x8B: adc<address_mode::e>(); break;
        case 0x8C: adc<address_mode::h>(); break;
        case 0x8D: adc<address_mode::l>(); break;
        case 0x8E: adc<address_mode::hl_indirect>(); break;
        case 0x8F: adc<address_mode::a>(); break;

        case 0x90: sub<address_mode::b>(); break;
        case 0x91: sub<address_mode::c>(); break;
        case 0x92: sub<address_mode::d>(); break;
        case 0x93: sub<address_mode::e>(); break;
        case 0x94: sub<address_mode::h>(); break;
        case 0x95: sub<address_mode::l>(); break;
        case 0x96: sub<address_mode::hl_indirect>(); break;
        case 0x97: sub<address_mode::a>(); break;
        case 0x98: sbc<address_mode::b>(); break;
        case 0x99: sbc<address_mode::c>(); break;
        case 0x9A: sbc<address_mode::d>(); break;
        case 0x9B: sbc<address_mode::e>(); break;
        case 0x9C: sbc<address_mode::h>(); break;
        case 0x9D: sbc<address_mode::l>(); break;
        case 0x9E: sbc<address_mode::hl_indirect>(); break;
        case 0x9F: sbc<address_mode::a>(); break;

        case 0xA0: land<address_mode::b>(); break;
        case 0xA1: land<address_mode::c>(); break;
        case 0xA2: land<address_mode::d>(); break;
        case 0xA3: land<address_mode::e>(); break;
        case 0xA4: land<address_mode::h>(); break;
        case 0xA5: land<address_mode::l>(); break;
        case 0xA6: land<address_mode::hl_indirect>(); break;
        case 0xA7: land<address_mode::a>(); break;
        case 0xA8: lxor<address_mode::b>(); break;
        case 0xA9: lxor<address_mode::c>(); break;
        case 0xAA: lxor<address_mode::d>(); break;
        case 0xAB: lxor<address_mode::e>(); break;
        case 0xAC: lxor<address_mode::h>(); break;
        case 0xAD: lxor<address_mode::l>(); break;
        case 0xAE: lxor<address_mode::hl_indirect>(); break;
        case 0xAF: lxor<address_mode::a>(); break;

        case 0xB0: lor<address_mode::b>(); break;
        case 0xB1: lor<address_mode::c>(); break;
        case 0xB2: lor<address_mode::d>(); break;
        case 0xB3: lor<address_mode::e>(); break;
        case 0xB4: lor<address_mode::h>(); break;
        case 0xB5: lor<address_mode::l>(); break;
        case 0xB6: lor<address_mode::hl_indirect>(); break;
        case 0xB7: lor<address_mode::a>(); break;
        case 0xB8: cp<address_mode::b>(); break;
        case 0xB9: cp<address_mode::c>(); break;
        case 0xBA: cp<address_mode::d>(); break;
        case 0xBB: cp<address_mode::e>(); break;
        case 0xBC: cp<address_mode::h>(); break;
        case 0xBD: cp<address_mode::l>(); break;
        case 0xBE: cp<address_mode::hl_indirect>(); break;
        case 0xBF: cp<address_mode::a>(); break;

        case 0xC0: ret<condition::nz>(); break;
        case 0xC1: pop<address_mode::bc>(); break;
        case 0xC2: jp<condition::nz>(); break;
        case 0xC3: jp(); break;
        case 0xC4: call<condition::nz>(); break;
        case 0xC5: push<address_mode::bc>(); break;
        case 0xC6: add<address_mode::imm_8>(); break;
        case 0xC7: rst<0x00>(); break;
        case 0xC8: ret<condition::z>(); break;
        case 0xC9: ret(); break;
        case 0xCA: jp<condition::z>(); break;
        case 0xCB: execute_cb(get_operand<address_mode::imm_8>()); break;
        case 0xCC: call<condition::z>(); break;
        case 0xCD: call(); break;
        case 0xCE: adc<address_mode::imm_8>(); break;
        case 0xCF: rst<0x08>(); break;

        case 0xD0: ret<condition::nc>(); break;
        case 0xD1: pop<address_mode::de>(); break;
        case 0xD2: jp<condition::nc>(); break;
        case 0xD4: call<condition::nc>(); break;
        case 0xD5: push<address_mode::de>(); break;
        case 0xD6: sub<address_mode::imm_8>(); break;
        case 0xD7: rst<0x10>(); break;
        case 0xD8: ret<condition::c>(); break;
        case 0xD9: reti(); break;
        case 0xDA: jp<condition::c>(); break;
        case 0xDC: call<condition::c>(); break;
        case 0xDE: sbc<address_mode::imm_8>(); break;
        case 0xDF: rst<0x18>(); break;

        case 0xE0: ldh<address_mode::imm_8, address_mode::a>(); break;
        case 0xE1: pop<address_mode::hl>(); break;
        case 0xE2: ldh<address_mode::c, address_mode::a>(); break;
        case 0xE5: push<address_mode::hl>(); break;
        case 0xE6: land<address_mode::imm_8>(); break;
        case 0xE7: rst<0x20>(); break;
        case 0xE8: ld_sp_e<address_mode::sp>(); break;
        case 0xE9: pc = get_operand<address_mode::hl>(); break; // JP HL
        case 0xEA: ld<address_mode::imm_16_indirect, address_mode::a>(); break;
        case 0xEE: lxor<address_mode::imm_8>(); break;
        case 0xEF: rst<0x28>(); break;

        case 0xF0: ldh<address_mode::a, address_mode::imm_8>(); break;
        case 0xF1: pop<address_mode::af>(); break;
        case 0xF2: ldh<address_mode::a, address_mode::c>(); break;
        case 0xF3: di(); break;
        case 0xF5: push<address_mode::af>(); break;
        case 0xF6: lor<address_mode::imm_8>(); break;
        case 0xF7: rst<0x30>(); break;
        case 0xF8: ld_sp_e<address_mode::hl>(); break;
        case 0xF9: ld_sp_hl(); break;
        case 0xFA: ld<address_mode::a, address_mode::imm_16_indirect>(); break;
        case 0xFB: ei(); break;
        case 0xFE: cp<address_mode::imm_8>(); break;
        case 0xFF: rst<0x38>(); break;

        // illegal opcodes TODO: these should hard-lock the entire system until a reset
        case 0xD3:
        case 0xDB:
        case 0xDD:
        case 0xE3:
        case 0xE4:
        case 0xEB:
        case 0xEC:
        case 0xED:
        case 0xF4:
        case 0xFC:
        case 0xFD:
        break;

        default: std::unreachable();
    }
    //@formatter:on
}

template<console model>
void cpu<model>::execute_cb(const u8 opcode)
{
    //@formatter:off
    switch (opcode) {
        case 0x00: rlc<address_mode::b>(); break;
        case 0x01: rlc<address_mode::c>(); break;
        case 0x02: rlc<address_mode::d>(); break;
        case 0x03: rlc<address_mode::e>(); break;
        case 0x04: rlc<address_mode::h>(); break;
        case 0x05: rlc<address_mode::l>(); break;
        case 0x06: rlc<address_mode::hl_indirect>(); break;
        case 0x07: rlc<address_mode::a>(); break;
        case 0x08: rrc<address_mode::b>(); break;
        case 0x09: rrc<address_mode::c>(); break;
        case 0x0A: rrc<address_mode::d>(); break;
        case 0x0B: rrc<address_mode::e>(); break;
        case 0x0C: rrc<address_mode::h>(); break;
        case 0x0D: rrc<address_mode::l>(); break;
        case 0x0E: rrc<address_mode::hl_indirect>(); break;
        case 0x0F: rrc<address_mode::a>(); break;

        case 0x10: rl<address_mode::b>(); break;
        case 0x11: rl<address_mode::c>(); break;
        case 0x12: rl<address_mode::d>(); break;
        case 0x13: rl<address_mode::e>(); break;
        case 0x14: rl<address_mode::h>(); break;
        case 0x15: rl<address_mode::l>(); break;
        case 0x16: rl<address_mode::hl_indirect>(); break;
        case 0x17: rl<address_mode::a>(); break;
        case 0x18: rr<address_mode::b>(); break;
        case 0x19: rr<address_mode::c>(); break;
        case 0x1A: rr<address_mode::d>(); break;
        case 0x1B: rr<address_mode::e>(); break;
        case 0x1C: rr<address_mode::h>(); break;
        case 0x1D: rr<address_mode::l>(); break;
        case 0x1E: rr<address_mode::hl_indirect>(); break;
        case 0x1F: rr<address_mode::a>(); break;

        case 0x20: sla<address_mode::b>(); break;
        case 0x21: sla<address_mode::c>(); break;
        case 0x22: sla<address_mode::d>(); break;
        case 0x23: sla<address_mode::e>(); break;
        case 0x24: sla<address_mode::h>(); break;
        case 0x25: sla<address_mode::l>(); break;
        case 0x26: sla<address_mode::hl_indirect>(); break;
        case 0x27: sla<address_mode::a>(); break;
        case 0x28: sra<address_mode::b>(); break;
        case 0x29: sra<address_mode::c>(); break;
        case 0x2A: sra<address_mode::d>(); break;
        case 0x2B: sra<address_mode::e>(); break;
        case 0x2C: sra<address_mode::h>(); break;
        case 0x2D: sra<address_mode::l>(); break;
        case 0x2E: sra<address_mode::hl_indirect>(); break;
        case 0x2F: sra<address_mode::a>(); break;

        case 0x30: swap<address_mode::b>(); break;
        case 0x31: swap<address_mode::c>(); break;
        case 0x32: swap<address_mode::d>(); break;
        case 0x33: swap<address_mode::e>(); break;
        case 0x34: swap<address_mode::h>(); break;
        case 0x35: swap<address_mode::l>(); break;
        case 0x36: swap<address_mode::hl_indirect>(); break;
        case 0x37: swap<address_mode::a>(); break;
        case 0x38: srl<address_mode::b>(); break;
        case 0x39: srl<address_mode::c>(); break;
        case 0x3A: srl<address_mode::d>(); break;
        case 0x3B: srl<address_mode::e>(); break;
        case 0x3C: srl<address_mode::h>(); break;
        case 0x3D: srl<address_mode::l>(); break;
        case 0x3E: srl<address_mode::hl_indirect>(); break;
        case 0x3F: srl<address_mode::a>(); break;

        case 0x40: bit<address_mode::b, 0>(); break;
        case 0x41: bit<address_mode::c, 0>(); break;
        case 0x42: bit<address_mode::d, 0>(); break;
        case 0x43: bit<address_mode::e, 0>(); break;
        case 0x44: bit<address_mode::h, 0>(); break;
        case 0x45: bit<address_mode::l, 0>(); break;
        case 0x46: bit<address_mode::hl_indirect, 0>(); break;
        case 0x47: bit<address_mode::a, 0>(); break;
        case 0x48: bit<address_mode::b, 1>(); break;
        case 0x49: bit<address_mode::c, 1>(); break;
        case 0x4A: bit<address_mode::d, 1>(); break;
        case 0x4B: bit<address_mode::e, 1>(); break;
        case 0x4C: bit<address_mode::h, 1>(); break;
        case 0x4D: bit<address_mode::l, 1>(); break;
        case 0x4E: bit<address_mode::hl_indirect, 1>(); break;
        case 0x4F: bit<address_mode::a, 1>(); break;

        case 0x50: bit<address_mode::b, 2>(); break;
        case 0x51: bit<address_mode::c, 2>(); break;
        case 0x52: bit<address_mode::d, 2>(); break;
        case 0x53: bit<address_mode::e, 2>(); break;
        case 0x54: bit<address_mode::h, 2>(); break;
        case 0x55: bit<address_mode::l, 2>(); break;
        case 0x56: bit<address_mode::hl_indirect, 2>(); break;
        case 0x57: bit<address_mode::a, 2>(); break;
        case 0x58: bit<address_mode::b, 3>(); break;
        case 0x59: bit<address_mode::c, 3>(); break;
        case 0x5A: bit<address_mode::d, 3>(); break;
        case 0x5B: bit<address_mode::e, 3>(); break;
        case 0x5C: bit<address_mode::h, 3>(); break;
        case 0x5D: bit<address_mode::l, 3>(); break;
        case 0x5E: bit<address_mode::hl_indirect, 3>(); break;
        case 0x5F: bit<address_mode::a, 3>(); break;

        case 0x60: bit<address_mode::b, 4>(); break;
        case 0x61: bit<address_mode::c, 4>(); break;
        case 0x62: bit<address_mode::d, 4>(); break;
        case 0x63: bit<address_mode::e, 4>(); break;
        case 0x64: bit<address_mode::h, 4>(); break;
        case 0x65: bit<address_mode::l, 4>(); break;
        case 0x66: bit<address_mode::hl_indirect, 4>(); break;
        case 0x67: bit<address_mode::a, 4>(); break;
        case 0x68: bit<address_mode::b, 5>(); break;
        case 0x69: bit<address_mode::c, 5>(); break;
        case 0x6A: bit<address_mode::d, 5>(); break;
        case 0x6B: bit<address_mode::e, 5>(); break;
        case 0x6C: bit<address_mode::h, 5>(); break;
        case 0x6D: bit<address_mode::l, 5>(); break;
        case 0x6E: bit<address_mode::hl_indirect, 5>(); break;
        case 0x6F: bit<address_mode::a, 5>(); break;

        case 0x70: bit<address_mode::b, 6>(); break;
        case 0x71: bit<address_mode::c, 6>(); break;
        case 0x72: bit<address_mode::d, 6>(); break;
        case 0x73: bit<address_mode::e, 6>(); break;
        case 0x74: bit<address_mode::h, 6>(); break;
        case 0x75: bit<address_mode::l, 6>(); break;
        case 0x76: bit<address_mode::hl_indirect, 6>(); break;
        case 0x77: bit<address_mode::a, 6>(); break;
        case 0x78: bit<address_mode::b, 7>(); break;
        case 0x79: bit<address_mode::c, 7>(); break;
        case 0x7A: bit<address_mode::d, 7>(); break;
        case 0x7B: bit<address_mode::e, 7>(); break;
        case 0x7C: bit<address_mode::h, 7>(); break;
        case 0x7D: bit<address_mode::l, 7>(); break;
        case 0x7E: bit<address_mode::hl_indirect, 7>(); break;
        case 0x7F: bit<address_mode::a, 7>(); break;

        case 0x80: res<address_mode::b, 0>(); break;
        case 0x81: res<address_mode::c, 0>(); break;
        case 0x82: res<address_mode::d, 0>(); break;
        case 0x83: res<address_mode::e, 0>(); break;
        case 0x84: res<address_mode::h, 0>(); break;
        case 0x85: res<address_mode::l, 0>(); break;
        case 0x86: res<address_mode::hl_indirect, 0>(); break;
        case 0x87: res<address_mode::a, 0>(); break;
        case 0x88: res<address_mode::b, 1>(); break;
        case 0x89: res<address_mode::c, 1>(); break;
        case 0x8A: res<address_mode::d, 1>(); break;
        case 0x8B: res<address_mode::e, 1>(); break;
        case 0x8C: res<address_mode::h, 1>(); break;
        case 0x8D: res<address_mode::l, 1>(); break;
        case 0x8E: res<address_mode::hl_indirect, 1>(); break;
        case 0x8F: res<address_mode::a, 1>(); break;

        case 0x90: res<address_mode::b, 2>(); break;
        case 0x91: res<address_mode::c, 2>(); break;
        case 0x92: res<address_mode::d, 2>(); break;
        case 0x93: res<address_mode::e, 2>(); break;
        case 0x94: res<address_mode::h, 2>(); break;
        case 0x95: res<address_mode::l, 2>(); break;
        case 0x96: res<address_mode::hl_indirect, 2>(); break;
        case 0x97: res<address_mode::a, 2>(); break;
        case 0x98: res<address_mode::b, 3>(); break;
        case 0x99: res<address_mode::c, 3>(); break;
        case 0x9A: res<address_mode::d, 3>(); break;
        case 0x9B: res<address_mode::e, 3>(); break;
        case 0x9C: res<address_mode::h, 3>(); break;
        case 0x9D: res<address_mode::l, 3>(); break;
        case 0x9E: res<address_mode::hl_indirect, 3>(); break;
        case 0x9F: res<address_mode::a, 3>(); break;

        case 0xA0: res<address_mode::b, 4>(); break;
        case 0xA1: res<address_mode::c, 4>(); break;
        case 0xA2: res<address_mode::d, 4>(); break;
        case 0xA3: res<address_mode::e, 4>(); break;
        case 0xA4: res<address_mode::h, 4>(); break;
        case 0xA5: res<address_mode::l, 4>(); break;
        case 0xA6: res<address_mode::hl_indirect, 4>(); break;
        case 0xA7: res<address_mode::a, 4>(); break;
        case 0xA8: res<address_mode::b, 5>(); break;
        case 0xA9: res<address_mode::c, 5>(); break;
        case 0xAA: res<address_mode::d, 5>(); break;
        case 0xAB: res<address_mode::e, 5>(); break;
        case 0xAC: res<address_mode::h, 5>(); break;
        case 0xAD: res<address_mode::l, 5>(); break;
        case 0xAE: res<address_mode::hl_indirect, 5>(); break;
        case 0xAF: res<address_mode::a, 5>(); break;

        case 0xB0: res<address_mode::b, 6>(); break;
        case 0xB1: res<address_mode::c, 6>(); break;
        case 0xB2: res<address_mode::d, 6>(); break;
        case 0xB3: res<address_mode::e, 6>(); break;
        case 0xB4: res<address_mode::h, 6>(); break;
        case 0xB5: res<address_mode::l, 6>(); break;
        case 0xB6: res<address_mode::hl_indirect, 6>(); break;
        case 0xB7: res<address_mode::a, 6>(); break;
        case 0xB8: res<address_mode::b, 7>(); break;
        case 0xB9: res<address_mode::c, 7>(); break;
        case 0xBA: res<address_mode::d, 7>(); break;
        case 0xBB: res<address_mode::e, 7>(); break;
        case 0xBC: res<address_mode::h, 7>(); break;
        case 0xBD: res<address_mode::l, 7>(); break;
        case 0xBE: res<address_mode::hl_indirect, 7>(); break;
        case 0xBF: res<address_mode::a, 7>(); break;

        case 0xC0: set<address_mode::b, 0>(); break;
        case 0xC1: set<address_mode::c, 0>(); break;
        case 0xC2: set<address_mode::d, 0>(); break;
        case 0xC3: set<address_mode::e, 0>(); break;
        case 0xC4: set<address_mode::h, 0>(); break;
        case 0xC5: set<address_mode::l, 0>(); break;
        case 0xC6: set<address_mode::hl_indirect, 0>(); break;
        case 0xC7: set<address_mode::a, 0>(); break;
        case 0xC8: set<address_mode::b, 1>(); break;
        case 0xC9: set<address_mode::c, 1>(); break;
        case 0xCA: set<address_mode::d, 1>(); break;
        case 0xCB: set<address_mode::e, 1>(); break;
        case 0xCC: set<address_mode::h, 1>(); break;
        case 0xCD: set<address_mode::l, 1>(); break;
        case 0xCE: set<address_mode::hl_indirect, 1>(); break;
        case 0xCF: set<address_mode::a, 1>(); break;

        case 0xD0: set<address_mode::b, 2>(); break;
        case 0xD1: set<address_mode::c, 2>(); break;
        case 0xD2: set<address_mode::d, 2>(); break;
        case 0xD3: set<address_mode::e, 2>(); break;
        case 0xD4: set<address_mode::h, 2>(); break;
        case 0xD5: set<address_mode::l, 2>(); break;
        case 0xD6: set<address_mode::hl_indirect, 2>(); break;
        case 0xD7: set<address_mode::a, 2>(); break;
        case 0xD8: set<address_mode::b, 3>(); break;
        case 0xD9: set<address_mode::c, 3>(); break;
        case 0xDA: set<address_mode::d, 3>(); break;
        case 0xDB: set<address_mode::e, 3>(); break;
        case 0xDC: set<address_mode::h, 3>(); break;
        case 0xDD: set<address_mode::l, 3>(); break;
        case 0xDE: set<address_mode::hl_indirect, 3>(); break;
        case 0xDF: set<address_mode::a, 3>(); break;

        case 0xE0: set<address_mode::b, 4>(); break;
        case 0xE1: set<address_mode::c, 4>(); break;
        case 0xE2: set<address_mode::d, 4>(); break;
        case 0xE3: set<address_mode::e, 4>(); break;
        case 0xE4: set<address_mode::h, 4>(); break;
        case 0xE5: set<address_mode::l, 4>(); break;
        case 0xE6: set<address_mode::hl_indirect, 4>(); break;
        case 0xE7: set<address_mode::a, 4>(); break;
        case 0xE8: set<address_mode::b, 5>(); break;
        case 0xE9: set<address_mode::c, 5>(); break;
        case 0xEA: set<address_mode::d, 5>(); break;
        case 0xEB: set<address_mode::e, 5>(); break;
        case 0xEC: set<address_mode::h, 5>(); break;
        case 0xED: set<address_mode::l, 5>(); break;
        case 0xEE: set<address_mode::hl_indirect, 5>(); break;
        case 0xEF: set<address_mode::a, 5>(); break;

        case 0xF0: set<address_mode::b, 6>(); break;
        case 0xF1: set<address_mode::c, 6>(); break;
        case 0xF2: set<address_mode::d, 6>(); break;
        case 0xF3: set<address_mode::e, 6>(); break;
        case 0xF4: set<address_mode::h, 6>(); break;
        case 0xF5: set<address_mode::l, 6>(); break;
        case 0xF6: set<address_mode::hl_indirect, 6>(); break;
        case 0xF7: set<address_mode::a, 6>(); break;
        case 0xF8: set<address_mode::b, 7>(); break;
        case 0xF9: set<address_mode::c, 7>(); break;
        case 0xFA: set<address_mode::d, 7>(); break;
        case 0xFB: set<address_mode::e, 7>(); break;
        case 0xFC: set<address_mode::h, 7>(); break;
        case 0xFD: set<address_mode::l, 7>(); break;
        case 0xFE: set<address_mode::hl_indirect, 7>(); break;
        case 0xFF: set<address_mode::a, 7>(); break;
        default: std::unreachable();
    }
    //@formatter:on
}

#ifndef NDEBUG
template<console model>
std::string cpu<model>::to_string()
{
    return std::format(
        "PC: {:0>4X}, A: {:0>2X}, F: {:x}{:x}{:x}{:x}, BC: {:0>4X}, DE: {:0>4X}, HL: {:0>4X}, SP: {:0>4X} \tram=({:0>2X} {:0>2X} {:0>2X} {:0>2X})",
        pc,
        reg[a],
        zf, nf, hf, cf,
        get_pair<b, c>(),
        get_pair<d, e>(),
        get_pair<h, l>(),
        sp,
        pc,
        mmu.ram[pc], mmu.ram[pc + 1U], mmu.ram[pc + 2U], mmu.ram[pc + 3U]);
}
#endif

template class cpu<console::dmg>;
template class cpu<console::cgb>;
