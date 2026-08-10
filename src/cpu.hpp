#pragma once

#ifndef NDEBUG
#include <format>
#include <string>
#endif
#include <type_traits>
#include "types.hpp"

template<console model>
class scheduler;

template<console model>
class mmu;

class interrupts;

template<console model>
class cpu {
public:
    cpu(scheduler<model>& s, mmu<model>& m, interrupts& i)
        : scheduler{s}, mmu{m}, interrupts{i}
    {}

    void tick();

#ifndef NDEBUG
    std::string to_string();
#endif

private:
    // Constants

    enum class address_mode {
        b, c,
        d, e,
        h, l,
        a,
        bc,
        de,
        hl,
        sp,
        af,
        bc_indirect,
        de_indirect,
        hl_indirect,
        hl_inc,
        hl_dec,
        imm_8,
        imm_16,
        imm_16_indirect
    };

    enum class condition {
        none,
        nz, z,
        nc, c
    };

    enum r {
        b = 0, c = 1,
        d = 2, e = 3,
        h = 4, l = 5,
        a = 7, f = 6
    };

    static constexpr u8 zbit{7};
    static constexpr u8 nbit{6};
    static constexpr u8 hbit{5};
    static constexpr u8 cbit{4};

    // CPU Internals

    scheduler<model>& scheduler;
    mmu<model>& mmu;
    interrupts& interrupts;
    u8 reg[8]{
        0x00,
        model == console::dmg ? 0x13 : 0x00,
        model == console::dmg ? 0x00 : 0xFF,
        model == console::dmg ? 0xD8 : 0x56,
        model == console::dmg ? 0x01 : 0x00,
        model == console::dmg ? 0x4D : 0x0D,
        0x00,
        model == console::dmg ? 0x01 : 0x11,
    };
    u16 sp{0xFFFE}, pc{0x100};
    bool zf{model == console::dmg}, nf{false}, hf{true}, cf{true};
    u8 ime{0};
    bool halted{false}, halt_bugged{false};

    template<auto M>
    static consteval bool is_8_bit();

    template<auto M>
    using operand_t = std::conditional_t<is_8_bit<M>(), u8, u16>;

    template<auto cc = condition::none>
    [[nodiscard]] bool evaluate_condition() const;

    template<auto hi, auto lo>
    [[nodiscard]] u16 get_pair() const;

    template<auto hi, auto lo>
    void set_pair(u16 val);

    template<auto M>
    [[nodiscard]] operand_t<M> get_operand();

    template<auto M>
    void set_operand(operand_t<M> data);

    void service_interrupt();

    [[nodiscard]] u8 fetch();

    void execute_cb(u8 opcode);

#pragma region Instructions // (https://rgbds.gbdev.io/docs/v0.9.3/gbz80.7)
    // Misc

    void stop();

    void halt();

    void di();

    void ei();

    // Load

    template<auto dst, auto src>
    void ld();

    template<auto dst, auto src>
    void ldh();

    template<auto M>
    void ld_sp_e();

    void ld_sp_hl();

    void ld_nn_sp();

    template<auto M>
    void pop();

    template<auto M>
    void push();

    // ALU

    template<auto M>
    void add();

    template<auto M>
    void adc();

    template<auto M>
    void sub();

    template<auto M>
    void sbc();

    template<auto M>
    void land();

    template<auto M>
    void lxor();

    template<auto M>
    void lor();

    template<auto M>
    void cp();

    template<auto M>
    void inc();

    template<auto M>
    void dec();

    // ALU helpers

    template<bool was_and>
    void set_logic_flags();

    template<bool addition>
    void do_arithmetic(u8 operand, bool cy = false);

    template<auto M, int sign>
    void do_increment();

    // Rotate/Shift, Bit

    template<auto M>
    void rlc();

    template<auto M>
    void rrc();

    template<auto M>
    void rl();

    template<auto M>
    void rr();

    template<auto M>
    void sla();

    template<auto M>
    void sra();

    template<auto M>
    void swap();

    template<auto M>
    void srl();

    template<auto M, u8 b3>
    void bit();

    template<auto M, u8 b3>
    void res();

    template<auto M, u8 b3>
    void set();

    // Rotate/Shift, Bit helpers

    void set_shift_flags(u8 val);

    // Accumulator/Flag

    void daa();

    void cpl();

    void scf();

    void ccf();

    void rlca();

    void rla();

    void rrca();

    void rra();

    // Branch

    template<auto cc = condition::none>
    void ret();

    void reti();

    template<auto cc = condition::none>
    void jp();

    template<auto cc = condition::none>
    void jr();

    template<auto cc = condition::none>
    void call();

    template<u8 tgt3>
    void rst();

    // Branch helpers

    void push_pc();
#pragma endregion
};
