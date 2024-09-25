#pragma once

#ifndef NDEBUG
#include <format>
#include <string>
#endif
#include "memory.hpp"
#include "types.hpp"

class cpu {
public:
    explicit cpu(memory& mem)
        : m_memory {mem}
    {}

    void tick();

#ifndef NDEBUG
    std::string to_string();
#endif

    bool low_power_mode {false};
private:
    // Constants

    static constexpr u8 b {0}, c {1};
    static constexpr u8 d {2}, e {3};
    static constexpr u8 h {4}, l {5};
    static constexpr u8 a {7}, f {6};

    static constexpr u8 zbit {7};
    static constexpr u8 nbit {6};
    static constexpr u8 hbit {5};
    static constexpr u8 cbit {4};

    // Registers/Flags

    memory& m_memory;
    u8 m_reg[8] {0x00, 0x13, 0x00, 0xD8, 0x01, 0x4D, 0, 0x01};
    u16 m_sp {0xFFFE}, m_pc {0x100};
    bool zf {true}, nf {false}, hf {true}, cf {true};

    // Getters/Setters

    template <u8 hi, u8 lo>
    [[nodiscard]] u16 get_pair() const;

    template <u8 hi, u8 lo>
    void set_pair(u16 val);

    [[nodiscard]] static u8 get_bit_mask(u8 val);
    [[nodiscard]] u8 get_imm8();
    [[nodiscard]] u16 get_imm16();

    template <u8 r8>
    [[nodiscard]] u8 get_r8();

    template <u8 r8>
    void set_r8(u8 data);

    enum class r16_group {
        base, // bc, de,  hl,  sp
        stk,  // bc, de,  hl,  af
        mem   // bc, de, hl+, hl-
    };

    template <u8 r16, r16_group group = r16_group::base>
    [[nodiscard]] u16 get_r16();

    template <u8 r16, r16_group group = r16_group::base>
    void set_r16(u16 data);

    // Instructions
    // ------------
    // Misc

    static void nop() {}
    void stop() {}
    void halt() {}
    void di() {}
    void ei() {}

    // Load/Store

    template<u8 op>
    [[nodiscard]] u16 get_ff00();

    template<u8 op, u8 bit = 8>
    void ld_r_imm();

    template<u8 op>
    void ld_r_r();

    template<u8 op>
    void ld_a();

    template<u8 op>
    void ldh();

    void ld_hl_sp();
    void ld_sp_hl();

    template<u8 op>
    void st_a();

    void st_sp();

    template<u8 op>
    void sth();

    template<u8 op>
    void pop();

    void push(u16 data);

    // Arithmetic Logic Unit

    template<u8 op>
    [[nodiscard]] u8 get_alu_operand();

    template<bool was_and>
    void set_logic_flags();

    template<bool addition>
    void do_arithmetic(u8 operand, bool cy = false);

    template<u8 op, u8 bit, int sign>
    void do_increment();

    template<u8 op, u8 bit = 8>
    void add();

    template<u8 op>
    void adc();

    template<u8 op>
    void sub();

    template<u8 op>
    void sbc();

    template<u8 op>
    void land();

    template<u8 op>
    void lxor();

    template<u8 op>
    void lor();

    template<u8 op>
    void cp();

    template<u8 op, u8 bit = 8>
    void inc();

    template<u8 op, u8 bit = 8>
    void dec();

    // Rotate/Shift Bit

    enum class shift_op {
        rlc  = 0b000,
        rrc  = 0b001,
        rl   = 0b010,
        rr   = 0b011,
        sla  = 0b100,
        sra  = 0b101,
        swap = 0b110,
        srl  = 0b111,
    };

    template<u8 op>
    void shift();

    template<u8 op>
    void bit();

    template<u8 op>
    void res();

    template<u8 op>
    void set();

    // Accumulator/Flag operations

    void daa();
    void cpl();
    void scf();
    void ccf();
    void rlca();
    void rla();
    void rrca();
    void rra();

    // Branch

    enum class cond {
        none,
        nz,
        z,
        nc,
        c
    };

    template<cond cc>
    [[nodiscard]] bool evaluate_cond() const;

    template<cond cc = cond::none>
    void ret();
    void reti();

    template<u8 op, cond cc = cond::none>
    void jp();

    template<cond cc = cond::none>
    void jr();

    template<cond cc = cond::none>
    void call();

    template<u8 op>
    void rst();
};