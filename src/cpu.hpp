#pragma once

#ifndef NDEBUG
#include <format>
#include <string>
#endif
#include <type_traits>
#include "types.hpp"

class scheduler;
class mmu;
class interrupts;

class cpu {
public:
    cpu(scheduler& s, mmu& m, interrupts& i)
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

    scheduler& scheduler;
    mmu& mmu;
    interrupts& interrupts;
    u8 reg[8]{0x00, 0x13, 0x00, 0xD8, 0x01, 0x4D, 0x00, 0x01};
    u16 sp{0xFFFE}, pc{0x100};
    bool zf{true}, nf{false}, hf{true}, cf{true};
    u8 ime{0};
    bool halted{false}, halt_bugged{false};

    template<address_mode M>
    static consteval bool is_8_bit();

    template<address_mode M>
    using operand_t = std::conditional_t<is_8_bit<M>(), u8, u16>;

    template<condition cc = condition::none>
    [[nodiscard]] bool evaluate_condition() const;

    template<r hi, r lo>
    [[nodiscard]] u16 get_pair() const;

    template<r hi, r lo>
    void set_pair(u16 val);

    template<address_mode M>
    [[nodiscard]] operand_t<M> get_operand();

    template<address_mode M>
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

    template<address_mode dst, address_mode src>
    void ld();

    template<address_mode dst, address_mode src>
    void ldh();

    template<address_mode M>
    void ld_sp_e();

    void ld_sp_hl();

    void ld_nn_sp();

    template<address_mode M>
    void pop();

    template<address_mode M>
    void push();

    // ALU

    template<address_mode M>
    void add();

    template<address_mode M>
    void adc();

    template<address_mode M>
    void sub();

    template<address_mode M>
    void sbc();

    template<address_mode M>
    void land();

    template<address_mode M>
    void lxor();

    template<address_mode M>
    void lor();

    template<address_mode M>
    void cp();

    template<address_mode M>
    void inc();

    template<address_mode M>
    void dec();

    // ALU helpers

    template<bool was_and>
    void set_logic_flags();

    template<bool addition>
    void do_arithmetic(u8 operand, bool cy = false);

    template<address_mode M, int sign>
    void do_increment();

    // Rotate/Shift, Bit

    template<address_mode M>
    void rlc();

    template<address_mode M>
    void rrc();

    template<address_mode M>
    void rl();

    template<address_mode M>
    void rr();

    template<address_mode M>
    void sla();

    template<address_mode M>
    void sra();

    template<address_mode M>
    void swap();

    template<address_mode M>
    void srl();

    template<address_mode M, u8 b3>
    void bit();

    template<address_mode M, u8 b3>
    void res();

    template<address_mode M, u8 b3>
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

    template<condition cc = condition::none>
    void ret();

    void reti();

    template<condition cc = condition::none>
    void jp();

    template<condition cc = condition::none>
    void jr();

    template<condition cc = condition::none>
    void call();

    template<u8 tgt3>
    void rst();

    // Branch helpers

    void push_pc();
#pragma endregion
};
