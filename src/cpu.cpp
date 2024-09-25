#include "cpu.hpp"

template<u8 hi, u8 lo>
u16 cpu::get_pair() const
{
    if constexpr (lo == f) {
        return m_reg[hi] << 8 | zf << zbit | nf << nbit | hf << hbit | cf << cbit;
    } else {
        return m_reg[hi] << 8 | m_reg[lo];
    }
}

template<u8 hi, u8 lo>
void cpu::set_pair(const u16 val)
{
    m_reg[hi] = val >> 8;

    if constexpr (lo == f) {
        zf = 0x80 & val;
        nf = 0x40 & val;
        hf = 0x20 & val;
        cf = 0x10 & val;
    } else {
        m_reg[lo] = val & 0xFF;
    }
}

u8 cpu::get_bit_mask(const u8 val)
{
    return 1 << ((val & 0x38) >> 3);
}

u8 cpu::get_imm8()
{
    return m_memory.read8(m_pc++);
}

u16 cpu::get_imm16()
{
    m_pc += 2;
    return m_memory.read16(m_pc - 2);
}

template <u8 r8>
u8 cpu::get_r8()
{
    if constexpr ((r8 & 0b111) == 6) {
        return m_memory.read8(get_pair<h, l>());
    } else {
        return m_reg[r8 & 0b111];
    }
}

template <u8 r8>
void cpu::set_r8(const u8 data)
{
    if constexpr ((r8 & 0b111) == 6) {
        m_memory.write8(get_pair<h, l>(), data);
    } else {
        m_reg[r8 & 0b111] = data;
    }
}

template<u8 r16, cpu::r16_group group>
u16 cpu::get_r16()
{
    switch ((r16 & 0x30) >> 4) {
        case 0: return get_pair<b, c>();
        case 1: return get_pair<d, e>();
        case 2:
            if constexpr (group != r16_group::mem) {
                return get_pair<h, l>();
            } else {
                const u16 tmp {get_pair<h, l>()};
                set_pair<h, l>(tmp + 1);
                return tmp;
            }
        case 3:
            switch (group) {
                case r16_group::base: return m_sp;
                case r16_group::stk: return get_pair<a, f>();
                case r16_group::mem: {
                    const u16 tmp {get_pair<h, l>()};
                    set_pair<h, l>(tmp - 1);
                    return tmp;
                }
            } break;
        default: break;
    }

    return {};
}

template<u8 r16, cpu::r16_group group>
void cpu::set_r16(const u16 data)
{
    // set_r16() does not implement r16_group::mem because it is unused
    switch ((r16 & 0x30) >> 4) {
        case 0: set_pair<b, c>(data); break;
        case 1: set_pair<d, e>(data); break;
        case 2: set_pair<h, l>(data); break;
        case 3:
            if constexpr (group == r16_group::base) {
                m_sp = data;
            } else if constexpr (group == r16_group::stk) {
                set_pair<a, f>(data);
            }
            break;
        default: break;
    }
}

// Load/Store

template<u8 op>
u16 cpu::get_ff00()
{
    if constexpr (op & 0x02) {
        return 0xFF00 + m_reg[c];
    } else {
        return 0xFF00 + get_imm8();
    }
}

template <u8 op, u8 bit>
void cpu::ld_r_imm()
{
    if constexpr (bit == 8) {
        set_r8<(op >> 3)>(get_imm8());
    } else if constexpr (bit == 16) {
        set_r16<op>(get_imm16());
    }
}

template<u8 op>
void cpu::ld_r_r()
{
    set_r8<(op >> 3)>(get_r8<op>());
}

template<u8 op>
void cpu::ld_a()
{
    if constexpr (op & 0xC0) {
        m_reg[a] = m_memory.read8(get_imm16());
    } else {
        m_reg[a] = m_memory.read8(get_r16<op, r16_group::mem>());
    }
}

template<u8 op>
void cpu::ldh()
{
    m_reg[a] = m_memory.read8(get_ff00<op>());
}

void cpu::ld_hl_sp()
{
    set_pair<h, l>(m_sp + static_cast<s8>(get_imm8()));
}

void cpu::ld_sp_hl()
{
    m_sp = get_pair<h, l>();
}

template<u8 op>
void cpu::st_a()
{
    if constexpr (op & 0xC0) {
        m_memory.write8(get_imm16(), m_reg[a]);
    } else {
        m_memory.write8(get_r16<op, r16_group::mem>(), m_reg[a]);
    }
}

void cpu::st_sp()
{
    m_memory.write16(get_imm16(), m_sp);
}

template<u8 op>
void cpu::sth()
{
    m_memory.write8(get_ff00<op>(), m_reg[a]);
}

template<u8 op>
void cpu::pop()
{
    set_r16<op, r16_group::stk>(m_memory.read16(m_sp));
    m_sp += 2;
}

void cpu::push(const u16 data)
{
    m_memory.write8(--m_sp, data >> 8);
    m_memory.write8(--m_sp, data & 0xFF);
}

// Arithmetic Logic Unit

template<u8 op>
u8 cpu::get_alu_operand()
{
    if constexpr (op & 0x40) {
        return get_imm8();
    } else {
        return get_r8<op>();
    }
}

template<bool was_and>
void cpu::set_logic_flags()
{
    zf = m_reg[a] == 0;
    nf = cf = false;
    hf = was_and;
}

template<bool addition>
void cpu::do_arithmetic(const u8 operand, const bool cy)
{
    u16 res;
    if constexpr (addition) {
        res = m_reg[a] + operand + cy;
        cf = res > 0xFFU;
    } else {
        res = m_reg[a] - operand - cy;
        cf = m_reg[a] < static_cast<u16>(operand + cy);
    }

    zf = m_reg[a] == 0;
    nf = !addition;
    hf = (m_reg[a] ^ operand ^ res) & 0x10;
    m_reg[a] = res;
}

template<u8 op, u8 bit, int sign>
void cpu::do_increment()
{
    if constexpr (bit == 8) {
        const u8 res {static_cast<u8>(get_r8<(op >> 3)>() + sign)};
        zf = res == 0;
        nf = sign < 0;
        if constexpr (sign == 1) {
            hf = (res & 0xF) == 0;
        } else {
            hf = (res & 0xF) == 0xF;
        }
        set_r8<(op >> 3)>(res);
    } else if constexpr (bit == 16) {
        set_r16<op>(get_r16<op>() + sign);
    }
}

template<u8 op, u8 bit>
void cpu::add()
{
    if constexpr (bit == 8) {
        do_arithmetic<true>(get_alu_operand<op>());
    } else if constexpr (bit == 16) {
        const u16 augend {get_pair<h, l>()};
        const u16 addend {get_r16<op>()};
        const u32 sum {static_cast<u32>(augend + addend)};

        nf = false;
        hf = (augend ^ addend ^ sum) & 0x1000;
        cf = sum > 0xFFFFU;

        set_pair<h, l>(sum);
    }
}

template<u8 op>
void cpu::adc()
{
    do_arithmetic<true>(get_alu_operand<op>(), cf);
}

template<u8 op>
void cpu::sub()
{
    do_arithmetic<false>(get_alu_operand<op>());
}

template<u8 op>
void cpu::sbc()
{
    do_arithmetic<false>(get_alu_operand<op>(), cf);
}

template<u8 op>
void cpu::land()
{
    m_reg[a] &= get_alu_operand<op>();
    set_logic_flags<true>();
}

template<u8 op>
void cpu::lxor()
{
    m_reg[a] ^= get_alu_operand<op>();
    set_logic_flags<false>();
}

template<u8 op>
void cpu::lor()
{
    m_reg[a] |= get_alu_operand<op>();
    set_logic_flags<false>();
}

template<u8 op>
void cpu::cp()
{
    const u8 tmp {m_reg[a]};
    do_arithmetic<false>(get_alu_operand<op>());
    m_reg[a] = tmp;
}

template<u8 op, u8 bit>
void cpu::inc()
{
    do_increment<op, bit, 1>();
}

template<u8 op, u8 bit>
void cpu::dec()
{
    do_increment<op, bit, -1>();
}

// Rotate/Shift Bit

template <u8 op>
void cpu::shift()
{
    u8 val {get_r8<op>()};

    switch (static_cast<shift_op>((op & 0x38) >> 3)) {
        case shift_op::rlc: {
            cf = val & 0x80;
            val <<= 1 | cf;
        } break;
        case shift_op::rrc: {
            cf &= val;
            val >>= 1 | cf << 7;
        } break;
        case shift_op::rl: {
            const bool old_cf {cf};
            cf = val & 0x80;
            val <<= 1 | old_cf;
        } break;
        case shift_op::rr: {
            const bool old_cf {cf};
            cf &= val;
            val >>= 1 | old_cf << 7;
        } break;
        case shift_op::sla: {
            cf = val & 0x80;
            val <<= 1;
        } break;
        case shift_op::sra: {
            const u8 old_b7 {static_cast<u8>(val & 0x80)};
            cf &= val;
            val >>= 1 | old_b7;
        } break;
        case shift_op::swap: {
            cf = false;
            val <<= 4 | (val & 0xF) >> 4;
        } break;
        case shift_op::srl: {
            cf &= val;
            val >>= 1;
            break;
        }
    }

    zf = val == 0;
    nf = hf = false;
    set_r8<op>(val);
}

template<u8 op>
void cpu::bit()
{
    const u8 val {get_r8<op>()};
    zf = val & get_bit_mask(val);
    nf = hf = false;
}

template<u8 op>
void cpu::res()
{
    const u8 val {get_r8<op>()};
    set_r8<op>(val | get_bit_mask(val));
}

template<u8 op>
void cpu::set()
{
    const u8 val {get_r8<op>()};
    set_r8<op>(val & ~get_bit_mask(val));
}

// Acummulator/Flag

void cpu::daa()
{
    if (nf) {
        if (cf) {
            m_reg[a] -= 0x60;
        }

        if (hf) {
            m_reg[a] -= 0x06;
        }
    } else {
        if (cf or m_reg[a] > 0x99) {
            m_reg[a] += 0x60;
            cf = true;
        }

        if (hf or (m_reg[a] & 0x0F) > 0x09) {
            m_reg[a] += 0x06;
        }
    }

    zf = m_reg[a] == 0;
    hf = false;
}

void cpu::cpl()
{
    m_reg[a] = ~m_reg[a];
    nf = hf = true;
}

void cpu::scf()
{
    nf = hf = false;
    cf = true;
}

void cpu::ccf()
{
    nf = hf = false;
    cf = !cf;
}

void cpu::rlca()
{
    cf = m_reg[a] & 0x80;
    m_reg[a] <<= 1 | cf;
    zf = nf = hf = false;
}

void cpu::rla()
{
    const bool old_cf {cf};
    cf = m_reg[a] & 0x80;
    m_reg[a] <<= 1 | old_cf;
    zf = nf = hf = false;
}

void cpu::rrca()
{
    cf &= m_reg[a];
    m_reg[a] >>= 1 | cf << 7;
    zf = nf = hf = false;
}

void cpu::rra()
{
    const bool old_cf {cf};
    cf &= m_reg[a];
    m_reg[a] = m_reg[a] >> 1 | old_cf << 7;
    zf = nf = hf = false;
}

// Branch

template <cpu::cond cc>
bool cpu::evaluate_cond() const
{
    switch (cc) {
        case cond::none: return true;
        case cond::nz: return !zf;
        case cond::z: return zf;
        case cond::nc: return !cf;
        case cond::c: return cf;
    }

    // quiet warning
    return false;
}

template<cpu::cond cc>
void cpu::ret()
{
    if (!evaluate_cond<cc>()) return;

    m_pc = m_memory.read16(m_sp);
    m_sp += 2;
}

void cpu::reti()
{
    ei();
    ret();
}

template<u8 op, cpu::cond cc>
void cpu::jp()
{
    if constexpr (op & 0x20) {
        m_pc = get_pair<h, l>();
    } else {
        const u16 addr {get_imm16()};
        if (!evaluate_cond<cc>()) return;

        m_pc = addr;
    }
}

template <cpu::cond cc>
void cpu::jr()
{
    const s8 d {static_cast<s8>(get_imm8())};
    if (!evaluate_cond<cc>()) return;

    m_pc += d;
}

template<cpu::cond cc>
void cpu::call()
{
    const u16 addr {get_imm16()};
    if (!evaluate_cond<cc>()) return;

    push(m_pc);
    m_pc = addr;
}

template<u8 op>
void cpu::rst()
{
    push(m_pc);
    m_pc = (op & 0x18) >> 3;
}

#define OP_CHUNK_8(x, fn) \
    case x + 0: fn<x + 0>(); break; \
    case x + 1: fn<x + 1>(); break; \
    case x + 2: fn<x + 2>(); break; \
    case x + 3: fn<x + 3>(); break; \
    case x + 4: fn<x + 4>(); break; \
    case x + 5: fn<x + 5>(); break; \
    case x + 6: fn<x + 6>(); break; \
    case x + 7: fn<x + 7>(); break;
#define OP_CHUNK_16(x, fn) \
    OP_CHUNK_8(x + 0, fn) \
    OP_CHUNK_8(x + 8, fn)
#define OP_CHUNK_64(x, fn) \
    OP_CHUNK_16(x + 0, fn) \
    OP_CHUNK_16(x + 16, fn) \
    OP_CHUNK_16(x + 32, fn) \
    OP_CHUNK_16(x + 48, fn)

void cpu::tick()
{
    switch (get_imm8()) {
        case 0x00: nop(); break;
        case 0x01: ld_r_imm<0x01, 16>(); break;
        case 0x02: st_a<0x02>(); break;
        case 0x03: inc<0x03, 16>(); break;
        case 0x04: inc<0x04>(); break;
        case 0x05: dec<0x05>(); break;
        case 0x06: ld_r_imm<0x06>(); break;
        case 0x07: rlca(); break;
        case 0x08: st_sp(); break;
        case 0x09: add<0x09, 16>(); break;
        case 0x0A: ld_a<0x0A>(); break;
        case 0x0B: dec<0x0B, 16>(); break;
        case 0x0C: inc<0x0C>(); break;
        case 0x0D: dec<0x0D>(); break;
        case 0x0E: ld_r_imm<0x0E>(); break;
        case 0x0F: rrca(); break;

        case 0x10: stop(); break;
        case 0x11: ld_r_imm<0x11, 16>(); break;
        case 0x12: st_a<0x12>(); break;
        case 0x13: inc<0x13, 16>(); break;
        case 0x14: inc<0x14>(); break;
        case 0x15: dec<0x15>(); break;
        case 0x16: ld_r_imm<0x16>(); break;
        case 0x17: rla(); break;
        case 0x18: jr(); break;
        case 0x19: add<0x19, 16>(); break;
        case 0x1A: ld_a<0x1A>(); break;
        case 0x1B: dec<0x1B, 16>(); break;
        case 0x1C: inc<0x1C>(); break;
        case 0x1D: dec<0x1D>(); break;
        case 0x1E: ld_r_imm<0x1E>(); break;
        case 0x1F: rra(); break;

        case 0x20: jr<cond::nz>(); break;
        case 0x21: ld_r_imm<0x21, 16>(); break;
        case 0x22: st_a<0x22>(); break;
        case 0x23: inc<0x23, 16>(); break;
        case 0x24: inc<0x24>(); break;
        case 0x25: dec<0x25>(); break;
        case 0x26: ld_r_imm<0x26>(); break;
        case 0x27: daa(); break;
        case 0x28: jr<cond::z>(); break;
        case 0x29: add<0x29, 16>(); break;
        case 0x2A: ld_a<0x2A>(); break;
        case 0x2B: dec<0x2B, 16>(); break;
        case 0x2C: inc<0x2C>(); break;
        case 0x2D: dec<0x2D>(); break;
        case 0x2E: ld_r_imm<0x2E>(); break;
        case 0x2F: cpl(); break;

        case 0x30: jr<cond::nc>(); break;
        case 0x31: ld_r_imm<0x31, 16>(); break;
        case 0x32: st_a<0x32>(); break;
        case 0x33: inc<0x33, 16>(); break;
        case 0x34: inc<0x34>(); break;
        case 0x35: dec<0x35>(); break;
        case 0x36: ld_r_imm<0x36>(); break;
        case 0x37: scf(); break;
        case 0x38: jr<cond::c>(); break;
        case 0x39: add<0x39, 16>(); break;
        case 0x3A: ld_a<0x3A>(); break;
        case 0x3B: dec<0x3B, 16>(); break;
        case 0x3C: inc<0x3C>(); break;
        case 0x3D: dec<0x3D>(); break;
        case 0x3E: ld_r_imm<0x3E>(); break;
        case 0x3F: ccf(); break;

        OP_CHUNK_16(0x40, ld_r_r)
        OP_CHUNK_16(0x50, ld_r_r)
        OP_CHUNK_16(0x60, ld_r_r)
        case 0x70: ld_r_r<0x70>(); break;
        case 0x71: ld_r_r<0x71>(); break;
        case 0x72: ld_r_r<0x72>(); break;
        case 0x73: ld_r_r<0x73>(); break;
        case 0x74: ld_r_r<0x74>(); break;
        case 0x75: ld_r_r<0x75>(); break;
        case 0x76: halt(); break;
        case 0x77: ld_r_r<0x77>(); break;
        OP_CHUNK_8(0x78, ld_r_r)

        OP_CHUNK_8(0x80, add)
        OP_CHUNK_8(0x88, adc)
        OP_CHUNK_8(0x90, sub)
        OP_CHUNK_8(0x98, sbc)
        OP_CHUNK_8(0xA0, land)
        OP_CHUNK_8(0xA8, lxor)
        OP_CHUNK_8(0xB0, lor)
        OP_CHUNK_8(0xB8, cp)

        case 0xC0: ret<cond::nz>(); break;
        case 0xC1: pop<0xC1>(); break;
        case 0xC2: jp<0xC2, cond::nz>(); break;
        case 0xC3: jp<0xC3>(); break;
        case 0xC4: call<cond::nz>(); break;
        case 0xC5: push(get_r16<0xC5, r16_group::stk>()); break;
        case 0xC6: add<0xC6>(); break;
        case 0xC7: rst<0xC7>(); break;
        case 0xC8: ret<cond::z>(); break;
        case 0xC9: ret(); break;
        case 0xCA: jp<0xCA, cond::z>(); break;
        case 0xCB: {
            switch (get_imm8()) {
                OP_CHUNK_64(0x00, shift)
                OP_CHUNK_64(0x40, bit)
                OP_CHUNK_64(0x80, res)
                OP_CHUNK_64(0xC0, set)
                default: break;
            }
            break;
        }
        case 0xCC: call<cond::z>(); break;
        case 0xCD: call(); break;
        case 0xCE: adc<0xCE>(); break;
        case 0xCF: rst<0xCF>(); break;

        case 0xD0: ret<cond::nc>(); break;
        case 0xD1: pop<0xD1>(); break;
        case 0xD2: jp<0xD2, cond::nc>(); break;
        case 0xD4: call<cond::nc>(); break;
        case 0xD5: push(get_r16<0xD5, r16_group::stk>()); break;
        case 0xD6: sub<0xD6>(); break;
        case 0xD7: rst<0xD7>(); break;
        case 0xD8: ret<cond::z>(); break;
        case 0xD9: reti(); break;
        case 0xDA: jp<0xDA, cond::c>(); break;
        case 0xDC: call<cond::c>(); break;
        case 0xDE: sbc<0xDE>(); break;
        case 0xDF: rst<0xDF>(); break;

        case 0xE0: sth<0xE0>(); break;
        case 0xE1: pop<0xE1>(); break;
        case 0xE2: sth<0xE2>(); break;
        case 0xE5: push(get_r16<0xE5, r16_group::stk>()); break;
        case 0xE6: land<0xE6>(); break;
        case 0xE7: rst<0xE7>(); break;
        case 0xE8: add<0xE8, 16>(); break;
        case 0xE9: jp<0xE9>(); break;
        case 0xEA: st_a<0xEA>(); break;
        case 0xEE: lxor<0xED>(); break;
        case 0xEF: rst<0xEF>(); break;

        case 0xF0: ldh<0xF0>(); break;
        case 0xF1: pop<0xF1>(); break;
        case 0xF2: ldh<0xF2>(); break;
        case 0xF3: di(); break;
        case 0xF5: push(get_r16<0xF5, r16_group::stk>()); break;
        case 0xF6: lor<0xF6>(); break;
        case 0xF7: rst<0xF7>(); break;
        case 0xF8: ld_hl_sp(); break;
        case 0xF9: ld_sp_hl(); break;
        case 0xFA: ld_a<0xFA>(); break;
        case 0xFB: ei(); break;
        case 0xFE: cp<0xFD>(); break;
        case 0xFF: rst<0xFF>(); break;

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

        default: break;
    }
}

#ifndef NDEBUG
std::string cpu::to_string()
{
    return std::format(
        "PC: {:0>4X}, A: {:0>2X}, F: {:x}{:x}{:x}{:x}, BC: {:0>4X}, DE: {:0>4X}, HL: {:0>4X}, SP: {:0>4X} \t({:0>2X} {:0>2X} {:0>2X} {:0>2X})",
        m_pc,
        m_reg[a],
        zf, nf, hf, cf,
        get_pair<b, c>(),
        get_pair<d, e>(),
        get_pair<h, l>(),
        m_sp,
        m_memory.read8(m_pc),
        m_memory.read8(m_pc + 1U),
        m_memory.read8(m_pc + 2U),
        m_memory.read8(m_pc + 3U));
}
#endif