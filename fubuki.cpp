
#include "helper.hpp"

#include <array>
#include <concepts>
#include <fstream>
#include <functional>
#include <limits>
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <stdint.h>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

// Another 6502 emulator because why not?
// http://www.obelisk.me.uk/6502/

namespace Fubuki {
using register_t = uint8_t;
using address_t = uint16_t;
using value_t = uint8_t;
using svalue_t = int8_t;
using address_diff_t = int16_t;

constexpr const auto default_memory_size = 0x10000ULL;
struct Memory {
    size_t m_size;
    std::unique_ptr<value_t[]> m_content;
    Memory()
        : m_size{default_memory_size}, m_content{std::make_unique_for_overwrite<value_t[]>(default_memory_size)} {};
    std::optional<value_t> load_byte(address_t address) const {
        if (address >= this->m_size) {
            return std::nullopt;
        }
        return m_content[address];
    }
    std::optional<address_t> load_address(address_t address) const {
        if (address >= m_size - 1) {
            return std::nullopt;
        }
        return m_content[address] | ((address_t)(m_content[address + 1]) << 8);
    }
    void set_byte(address_t address, value_t value) {
        if (address >= m_size) {
            return;
        }
        m_content[address] = value;
    }
    std::span<value_t> probe(address_t address, size_t request_size) const {
        size_t size = address > m_size ? 0 : std::min(m_size - address, request_size);
        return std::span{m_content.get() + address, size};
    }
};

struct Processor {
    /**
     * ARCHITECTURE
     * 8-bit processor
     * Some interal registers
     * 16-bit address bus (64Kb)
     * Little-endian
     *
     * First 256 bytes  $0000-$00FF - Zero Page
     * Special addressing mode/Indirect memory access
     * Second 256 bytes $0100-$01FF - Reserved
     * System stack. Cannot be reallocated
     * Last 6 bytes     $FFFA-$FFFF - Reserved
     * $FFFA/B non-maskable interrupt handler
     * $FFFC/D power on reset location
     * $FFFE/F BRK/interrupt request handler
     *
     * Map region(s) of memory to interface with other hardware
     */
    using memory_type = Memory;
    memory_type memory;

    /**
     * REGISTERS
     */
    using flag_type = bool;

    static constexpr size_t stage = 5;

    /**
     * Program Counter
     * 16-bit
     * Points to next instruction to be executed
     * Modified only by branch, jump, call, ret, interrupt
     */
    address_t pc;
    /**
     * Stack Pointer
     * 8-bit
     * Next free location on the stack
     * Stack grow negative (TODO: stack grow up?)
     * Assume no stack overflow
     */
    register_t sp;
    /**
     * Accumulator
     * 8-bit
     * Used in all arithmetic & logical ops (except inc and dec)
     * Load/store from memory or stack
     */
    register_t a;
    /**
     * Index register X
     * 8-bit
     * Usually hold counters or offsets for accessing memory
     * Load/save from memory. Compare with value in memory or inc or dec
     * Special: can copy or change SP
     */
    register_t x;
    /**
     * Index register Y
     * 8-bit
     * Usually hold counters or offsets for accessing memory
     * Load/save from memory. Compare with value in memory or inc or dec
     * Special: none (this is different from X)
     */
    register_t y;
    /**
     * Processor status
     */

    union processor_status_t {
        struct processor_status_flag_type {
            /**
             * Carry Flag
             * Result overflow from bit 7/underflow from bit 0.
             * Set during arithmetic, comparison, logical shift
             * Set if SEC executed, reset if CLC executed
             */
            flag_type C : 1;
            /**
             * Zero Flag
             * Result is 0 (zero)
             */
            flag_type Z : 1;
            /**
             * Interrupt Disable
             * Set if SEI executed, reset if CLI executed
             */
            flag_type I : 1;
            /**
             * Decimal Mode
             * Use BCD (binary coded decimal) arithmetic in ADD, SUB
             * Set if SED executed, reset if CLD executed
             */
            flag_type D : 1;
            /**
             * Break Command
             * Set if BRK is executed and interrupt has been generated
             */
            flag_type B : 1;
            /**
             * Overflow Flag
             * Invalid 2's compliment
             */
            flag_type V : 1;
            /**
             * Negative Flag
             * Result less than 0.
             * Bit 7 is set to 1
             */
            flag_type N : 1;

            register_t _reserved : 1;
        } flag;
        register_t regr;
    };

    processor_status_t ps;
};

constexpr inline address_t to_spaddr(register_t sp) {
    return 0x100 | sp;
}

constexpr inline address_t to_zpaddr(register_t sp) {
    return sp;
}

struct ProcessorChange {
    std::optional<address_t> pc;
    std::optional<register_t> sp;
    std::optional<register_t> a;
    std::optional<register_t> x;
    std::optional<register_t> y;
    std::optional<Processor::processor_status_t> ps;
    template <size_t size>
    static inline ProcessorChange nop(Processor const &proc) {
        return {.pc = proc.pc + size};
    }

    void commit_to(Processor &proc) const {

        if (this->pc) {
            proc.pc = *this->pc;
        }
        if (this->sp) {
            proc.sp = *this->sp;
        }
        if (this->a) {
            proc.a = *this->a;
        }
        if (this->x) {
            proc.x = *this->x;
        }
        if (this->y) {
            proc.y = *this->y;
        }
        if (this->ps) {
            proc.ps = *this->ps;
        }
    }
};

struct MemoryWrite {
    address_t address;
    value_t value;
};

struct MemoryWriteList {
    // Abstract away this ugly thing
    std::map<address_t, value_t> m_dir;

    MemoryWriteList() {};

    MemoryWriteList(MemoryWrite write) {
        // Note: this will always write to the map
        m_dir.emplace(write.address, write.value);
    }
    MemoryWriteList(std::initializer_list<MemoryWrite> writes) {
        for (auto &&write : writes) {
            m_dir.emplace(write.address, write.value);
        }
    }
    void commit_to(Processor &proc) const {
        for (auto const &[address, value] : m_dir) {
            proc.memory.set_byte(address, value);
        }
    }
};

template <typename T>
concept imm_ct = std::unsigned_integral<T>;
// #define imm_ct typename

/**
 * INSTRUCTION SET
 */
namespace Instruction {
struct Result {
    ProcessorChange m_pchange;
    MemoryWriteList m_write;
    // std::array<std::size_t, Processor::stage> cycle = {0};
    Result(ProcessorChange change) : m_pchange(change) {};
    Result(MemoryWrite write) : m_write{write} {};
    Result(ProcessorChange change, MemoryWrite write) : m_pchange(change), m_write{write} {};
    Result(ProcessorChange change, std::initializer_list<MemoryWrite> writes) : m_pchange(change), m_write{writes} {};

    friend Processor &operator<<(Processor &proc, Result const &change);
};

Processor &operator<<(Processor &proc, Result const &change) {
    change.m_pchange.commit_to(proc);
    change.m_write.commit_to(proc);
    return proc;
}

namespace AddressingType {
/**
 * Directly specifying 8-bit constant.
 * Example: `LDA #10`
 */
struct Immediate {
    using imm_type = value_t;
    constexpr static const size_t size{2};
};
/**
 * Implicitly use register A.
 * Example: `LSR A`, `LDX #LO LABEL`
 */
struct Accumulator {
    constexpr static const size_t size{1};
};
/**
 * Specifying 16-bit address in Zero Page using 8-bit constant offset.
 * Example: `LDA $00`, `ASL ANSWER`
 */
struct ZeroPage {
    using imm_type = value_t;
    constexpr static const size_t size{2};
};
/**
 * Specifying 16-bit address in Zero Page using 8-bit constant and register X offsets.
 * Example: `STY $10,X`, `AND TEMP,X`
 */
struct ZeroPageX {
    using imm_type = value_t;
    constexpr static const size_t size{2};
};
/**
 * Specifying 16-bit address in Zero Page using 8-bit constant and register Y offsets.
 * Example: `LDX $10,Y`, `STX TEMP,Y`
 */
struct ZeroPageY {
    using imm_type = value_t;
    constexpr static const size_t size{2};
};
/**
 * Specifying 16-bit absolute address
 * Example: `JMP $1234`, `JSR WIBBLE`
 */
struct Absolute {
    using imm_type = address_t;
    constexpr static const size_t size{3};
};
/**
 * Specifying 16-bit absolute address with offset register X
 * Example: `STA $3000,X`, `ROR CRC,X`
 */
struct AbsoluteX {
    using imm_type = address_t;
    constexpr static const size_t size{3};
};
/**
 * Specifying 16-bit absolute address with offset register Y
 * Example: `AND $4000,Y`, `STA MEM,Y`
 */
struct AbsoluteY {
    using imm_type = address_t;
    constexpr static const size_t size{3};
};
/**
 * Specifying 16-bit indirect address.
 * Used in JMP instruction only
 * Example: `JMP ($FFFC)`, `JMP (TARGET)`
 */
struct Indirect {
    using imm_type = address_t;
    constexpr static const size_t size{3};
};
/**aka Indexed Indirect
 * Specifying 16-bit indirect address with offset register X
 * Example: `LDA ($40,X)`, `STA (MEM,X)`
 */
struct IndirectX {
    using imm_type = value_t;
    constexpr static const size_t size{2};
};
/**aka Indirect Indexed
 * Specifying 16-bit indirect address with index register X
 * Example: `LDA ($40),Y`, `STA (DST,Y)`
 */
struct IndirectY {
    using imm_type = value_t;
    constexpr static const size_t size{2};
};
struct Implied {
    constexpr static const size_t size{1};
};
/**
 * Specifying 16-bit relative address
 * Used in branch instruction type only
 * Example: `BEQ LABEL`, `BNE *+4`
 */
struct Relative {
    using imm_type = int8_t;
    constexpr static const size_t size{2};
};
}; // namespace AddressingType

namespace Param {

struct Label {
    char store[256] = {0};
    // Label(Label &&rhs) {
    //     std::copy_n(rhs.store, sizeof(store), store);
    //     address = rhs.address;
    // }
    Label(std::string_view s) {
        std::copy_n(s.data(), std::min(sizeof(store) - 1, s.size()), store);
    }
};
struct Address {
    address_t store;
    Address(address_t v) : store(v) {};
};
struct Imm {
    value_t store;
    Imm(value_t v) : store(v) {};
};
struct Offset {
    svalue_t store;
    Offset(decltype(store) v) : store(v) {};
};
struct A {
    A() = default;
};
struct X {};
struct Y {};
struct Indirect {};
struct Implied {};

using AddressX = std::pair<Address, X>;
using LabelX = std::pair<Label, X>;
using AddressY = std::pair<Address, Y>;
using LabelY = std::pair<Label, Y>;
using IndirectAddress = std::pair<Indirect, Address>;
using IndirectLabel = std::pair<Indirect, Label>;
using IdxIndirectAddress = std::pair<std::pair<Indirect, X>, Address>;
using IdxIndirectLabel = std::pair<std::pair<Indirect, X>, Label>;
using IndirectIdxAddress = std::pair<std::pair<Indirect, Y>, Address>;
using IndirectIdxLabel = std::pair<std::pair<Indirect, Y>, Label>;

using Params = std::variant<std::monostate, A, Imm, Address, Label, Offset, Implied, AddressX, LabelX, AddressY, LabelY,
                            IndirectAddress, IndirectLabel, IdxIndirectAddress, IdxIndirectLabel, IndirectIdxAddress,
                            IndirectIdxLabel>;

Params parse(std::string_view raw_params) {
    auto clean = str_concat(raw_params | std::views::split(' '));
    if (clean.empty()) {
        return Implied{};
    }
    auto rough = to_vec(clean | std::views::split(',') | cast_sv);
    if (str_equal<"A">(rough[0])) {
        return A{};
    }
    auto &first = rough[0];
    auto psize = rough.size();
    // TODO: Support LabelLo and LabelHi
    if (first.starts_with('(')) {
        if (first.ends_with(')')) {
            if (first.length() > 3 && first[1] == '$') {
                address_t address = parse_int<address_t>(first.substr(2, first.length() - 3)).value();
                switch (psize) {
                case 1:
                    return std::make_pair(Indirect{}, Address{address});
                case 2:
                    if (str_equal<"Y">(rough[1])) {
                        return std::make_pair(std::make_pair(Indirect{}, Y{}), Address{address});
                    }
                }
            } else if (first.length() > 2) {
                return std::make_pair(Indirect{}, Label{first.substr(1, first.length() - 2)});
            }
        } else if (psize == 2 && str_equal<"X)">(rough[1])) {
            if (first.length() > 2 && first[1] == '$') {
                address_t address = parse_int<address_t>(first.substr(2)).value();
                return std::make_pair(std::make_pair(Indirect{}, X{}), Address{address});
            } else if (first.length() > 1) {
                return std::make_pair(std::make_pair(Indirect{}, X{}), Label{first.substr(1)});
            }
        }
    } else if (psize == 1 && first.starts_with('#')) {
        auto value = parse_int<value_t, 10>(first.substr(1)).value();
        return Imm{value};
    } else if (first.starts_with('$')) {
        address_t address = parse_int<address_t>(first.substr(1)).value();
        switch (psize) {
        case 1:
            return Address{address};
        case 2:
            if (str_equal<"X">(rough[1])) {
                return std::make_pair(Address{address}, X{});
            } else if (str_equal<"Y">(rough[1])) {
                return std::make_pair(Address{address}, Y{});
            }
        }
    } else {
        switch (psize) {
        case 1:
            return Label{first};
        case 2:
            if (str_equal<"X">(rough[1])) {
                return std::make_pair(Label{first}, X{});
            } else if (str_equal<"Y">(rough[1])) {
                return std::make_pair(Label{first}, Y{});
            }
        }
    }
    return std::monostate{};
}

} // namespace Param

// template <typename T>
// concept AddressingMode = is_one_of_v<T, AddressingType::Immediate, AddressingType::Accumulator,
//                                      AddressingType::ZeroPage, AddressingType::ZeroPageX, AddressingType::ZeroPageY,
//                                      AddressingType::Absolute, AddressingType::AbsoluteX, AddressingType::AbsoluteY,
//                                      AddressingType::Indirect, AddressingType::IndirectX, AddressingType::IndirectY,
//                                      AddressingType::Implied, AddressingType::Relative>;

// Hacky way
#define AddressingMode typename
template <typename T>
concept is_typeA = is_one_of_v<T, AddressingType::Immediate, AddressingType::ZeroPage, AddressingType::ZeroPageX,
                               AddressingType::Absolute, AddressingType::AbsoluteX, AddressingType::AbsoluteY,
                               AddressingType::IndirectX, AddressingType::IndirectY>;

template <AddressingMode am>
    requires requires() { typename am::imm_type; }
constexpr inline auto decode_AM_value(Processor const &proc, typename am::imm_type const &imm) {
    if constexpr (std::is_same_v<am, AddressingType::Immediate>) {
        return imm;
    } else if constexpr (std::is_same_v<am, AddressingType::ZeroPage>) {
        return proc.memory.load_byte(to_zpaddr(imm)).value();
    } else if constexpr (std::is_same_v<am, AddressingType::ZeroPageX>) {
        return proc.memory.load_byte(to_zpaddr(proc.x + imm)).value();
    } else if constexpr (std::is_same_v<am, AddressingType::ZeroPageY>) {
        return proc.memory.load_byte(to_zpaddr(proc.y + imm)).value();
    } else if constexpr (std::is_same_v<am, AddressingType::Absolute>) {
        return proc.memory.load_byte(imm).value();
    } else if constexpr (std::is_same_v<am, AddressingType::AbsoluteX>) {
        return proc.memory.load_byte(proc.x + imm).value();
    } else if constexpr (std::is_same_v<am, AddressingType::AbsoluteY>) {
        return proc.memory.load_byte(proc.y + imm).value();
    } else if constexpr (std::is_same_v<am, AddressingType::Indirect>) {
        return proc.memory.load_byte(proc.memory.load_address(imm).value()).value();
    } else if constexpr (std::is_same_v<am, AddressingType::IndirectX>) {
        return proc.memory.load_byte(proc.memory.load_address(proc.x + imm).value()).value();
    } else if constexpr (std::is_same_v<am, AddressingType::IndirectY>) {
        return proc.memory.load_byte(proc.y + proc.memory.load_address(imm).value()).value();
    }
    // If you are here, then you probably did the wrong thing
}

template <AddressingMode am>
    requires requires() { typename am::imm_type; }
constexpr inline auto decode_AM_address(Processor const &proc, typename am::imm_type const &imm) {
    if constexpr (std::is_same_v<am, AddressingType::ZeroPage>) {
        return to_zpaddr(imm);
    } else if constexpr (std::is_same_v<am, AddressingType::ZeroPageX>) {
        return to_zpaddr(proc.x + imm);
    } else if constexpr (std::is_same_v<am, AddressingType::ZeroPageY>) {
        return to_zpaddr(proc.y + imm);
    } else if constexpr (std::is_same_v<am, AddressingType::Absolute>) {
        return imm;
    } else if constexpr (std::is_same_v<am, AddressingType::AbsoluteX>) {
        return proc.x + imm;
    } else if constexpr (std::is_same_v<am, AddressingType::AbsoluteY>) {
        return proc.y + imm;
    } else if constexpr (std::is_same_v<am, AddressingType::Indirect>) {
        return proc.memory.load_address(imm).value();
    } else if constexpr (std::is_same_v<am, AddressingType::IndirectX>) {
        return proc.memory.load_address(proc.x + imm).value();
    } else if constexpr (std::is_same_v<am, AddressingType::IndirectY>) {
        return proc.y + proc.memory.load_address(imm).value();
    } else if constexpr (std::is_same_v<am, AddressingType::Relative>) {
        return static_cast<address_t>(proc.pc + imm);
    }
    // If you are here, then you probably did the wrong thing
}

/**
 * Load/Store operations
 */

template <AddressingMode am, size_t size>
    requires is_typeA<am>
struct LoadA {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = value == 0;
        new_status.flag.N = (value >> 7) & 1;
        ProcessorChange result = {.pc = proc.pc + size, .a = value, .ps = new_status};
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::Immediate, AddressingType::ZeroPage, AddressingType::ZeroPageY,
                         AddressingType::Absolute, AddressingType::AbsoluteY>
struct LoadX {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = value == 0;
        new_status.flag.N = (value >> 7) & 1;
        ProcessorChange result = {.pc = proc.pc + size, .x = value, .ps = new_status};
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::Immediate, AddressingType::ZeroPage, AddressingType::ZeroPageX,
                         AddressingType::Absolute, AddressingType::AbsoluteX>
struct LoadY {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = value == 0;
        new_status.flag.N = (value >> 7) & 1;
        ProcessorChange result = {.pc = proc.pc + size, .y = value, .ps = new_status};
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_typeA<am> && (not std::is_same_v<am, AddressingType::Immediate>)
struct StoreA {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        address_t address = decode_AM_address<am>(proc, imm);
        MemoryWrite write = {.address = address, .value = proc.a};
        return {ProcessorChange::nop<size>(proc), write};
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::ZeroPage, AddressingType::ZeroPageY, AddressingType::Absolute>
struct StoreX {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        address_t address = decode_AM_address<am>(proc, imm);
        MemoryWrite write = {.address = address, .value = proc.x};
        return {ProcessorChange::nop<size>(proc), write};
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::ZeroPage, AddressingType::ZeroPageX, AddressingType::Absolute>
struct StoreY {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        address_t address = decode_AM_address<am>(proc, imm);
        MemoryWrite write = {.address = address, .value = proc.y};
        return {ProcessorChange::nop<size>(proc), write};
    }
};

/**
 * Register Transfers
 */

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct TransferAX {
    Result operator()(Processor const &proc) {
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = proc.a == 0;
        new_status.flag.N = (proc.a >> 7) & 1;
        ProcessorChange result = {.pc = proc.pc + size, .x = proc.a, .ps = new_status};
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct TransferAY {
    Result operator()(Processor const &proc) {
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = proc.a == 0;
        new_status.flag.N = (proc.a >> 7) & 1;
        ProcessorChange result = {.pc = proc.pc + size, .y = proc.a, .ps = new_status};
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct TransferXA {
    Result operator()(Processor const &proc) {
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = proc.x == 0;
        new_status.flag.N = (proc.x >> 7) & 1;
        ProcessorChange result = {.pc = proc.pc + size, .a = proc.x, .ps = new_status};
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct TransferYA {
    Result operator()(Processor const &proc) {
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = proc.y == 0;
        new_status.flag.N = (proc.y >> 7) & 1;
        ProcessorChange result = {.pc = proc.pc + size, .a = proc.y, .ps = new_status};
        return result;
    }
};

/**
 * Stack Operations
 */

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct TransferSpX {
    Result operator()(Processor const &proc) {
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = proc.sp == 0;
        new_status.flag.N = (proc.sp >> 7) & 1;
        ProcessorChange result = {.pc = proc.pc + size, .x = proc.sp, .ps = new_status};
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct TransferXSp {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {.pc = proc.pc + size, .sp = proc.x};
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct PushA {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {.pc = proc.pc + size, .sp = proc.sp - 1};
        MemoryWrite write = {.address = to_spaddr(proc.sp), .value = proc.a};
        return {result, write};
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct PushPs {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {.pc = proc.pc + size, .sp = proc.sp - 1};
        MemoryWrite write = {.address = to_spaddr(proc.sp), .value = proc.ps.regr};
        return {result, write};
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct PullA {
    Result operator()(Processor const &proc) {
        auto value = proc.memory.load_byte(to_spaddr(proc.sp + 1)).value();
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = value == 0;
        new_status.flag.N = (value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .sp = proc.sp + 1,
            .a = value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct PullPs {
    Result operator()(Processor const &proc) {
        auto value = proc.memory.load_byte(to_spaddr(proc.sp + 1)).value();
        Processor::processor_status_t new_status;
        new_status.regr = value;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .sp = proc.sp + 1,
            .ps = new_status,
        };
        return result;
    }
};

/**
 * Logical
 */

template <AddressingMode am, size_t size>
    requires is_typeA<am>
struct LogicalAND {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = value == 0;
        new_status.flag.N = (value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .a = proc.a & value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_typeA<am>
struct LogicalEOR {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = value == 0;
        new_status.flag.N = (value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .a = proc.a ^ value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_typeA<am>
struct LogicalOR {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = value == 0;
        new_status.flag.N = (value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .a = proc.a | value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::ZeroPage, AddressingType::Absolute>
struct LogicalBitTest {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = 0 == (value & value);
        new_status.flag.N = (value >> 7) & 1;
        new_status.flag.V = (value >> 6) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .ps = new_status,
        };
        return result;
    }
};

/**
 * Arithmetic
 */

inline std::tuple<value_t, Processor::processor_status_t> _addWithCarry(value_t reg_a, value_t value,
                                                                        Processor::processor_status_t status) {
    constexpr auto top = std::numeric_limits<value_t>::max();
    value_t carry = status.flag.C;
    value_t sum = reg_a + value + carry;
    value_t new_carry = (reg_a == top && value == top) || ((reg_a < top) && (reg_a + carry > top - value)) ||
                        ((value < top) && (value + carry > top - reg_a));
    Processor::processor_status_t new_status = status;
    new_status.flag.C = new_carry;
    new_status.flag.Z = 0 == sum;
    new_status.flag.N = (sum >> 7) & 1;
    new_status.flag.V = ((reg_a >> 7) ^ (value >> 7) ^ 1) & ((value >> 7) ^ (sum >> 7)) & 1;
    return {sum, new_status};
}

template <AddressingMode am, size_t size>
    requires is_typeA<am>
struct AddWithCarry {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        auto &&[sum, new_status] = _addWithCarry(proc.a, value, proc.ps);
        ProcessorChange result = {
            .pc = proc.pc + size,
            .a = sum,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_typeA<am>
struct SubWithCarry {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = -decode_AM_value<am>(proc, imm);
        auto &&[sum, new_status] = _addWithCarry(proc.a, value, proc.ps);
        ProcessorChange result = {
            .pc = proc.pc + size,
            .a = sum,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_typeA<am>
struct CompareA {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = -decode_AM_value<am>(proc, imm);
        auto &&new_status = std::get<1>(_addWithCarry(proc.a, value, proc.ps));
        ProcessorChange result = {
            .pc = proc.pc + size,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::Immediate, AddressingType::ZeroPage, AddressingType::Absolute>
struct CompareX {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = -decode_AM_value<am>(proc, imm);
        auto &&new_status = std::get<1>(_addWithCarry(proc.x, value, proc.ps));
        ProcessorChange result = {
            .pc = proc.pc + size,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::Immediate, AddressingType::ZeroPage, AddressingType::Absolute>
struct CompareY {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = -decode_AM_value<am>(proc, imm);
        auto &&new_status = std::get<1>(_addWithCarry(proc.y, value, proc.ps));
        ProcessorChange result = {
            .pc = proc.pc + size,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::ZeroPage, AddressingType::ZeroPageX, AddressingType::Absolute,
                         AddressingType::AbsoluteX>
struct IncrementMemory {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        address_t address = decode_AM_address<am>(proc, imm);
        value_t new_value = value + 1U;
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .ps = new_status,
        };
        MemoryWrite write = {
            .address = address,
            .value = new_value,
        };
        return {result, write};
    }
};

/**
 * Increments & Decrements
 */

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct IncrementX {
    Result operator()(Processor const &proc) {
        value_t new_value = proc.x + 1U;
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .x = new_value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct IncrementY {
    Result operator()(Processor const &proc) {
        value_t new_value = proc.y + 1U;
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .y = new_value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::ZeroPage, AddressingType::ZeroPageX, AddressingType::Absolute,
                         AddressingType::AbsoluteX>
struct DecrementMemory {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        address_t address = decode_AM_address<am>(proc, imm);
        value_t new_value = value - 1U;
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .ps = new_status,
        };
        MemoryWrite write = {
            .address = address,
            .value = new_value,
        };
        return {result, write};
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct DecrementX {
    Result operator()(Processor const &proc) {
        value_t new_value = proc.x - 1U;
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .x = new_value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct DecrementY {
    Result operator()(Processor const &proc) {
        value_t new_value = proc.y - 1U;
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .y = new_value,
            .ps = new_status,
        };
        return result;
    }
};

/**
 * Shifts
 */

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::Accumulator>
struct ArithShiftLeftA {
    Result operator()(Processor const &proc) {
        value_t new_value = proc.a << 1;
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.C = (proc.a >> 7) & 1;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .a = new_value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::ZeroPage, AddressingType::ZeroPageX, AddressingType::Absolute,
                         AddressingType::AbsoluteX>
struct ArithShiftLeft {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        value_t new_value = value << 1;
        address_t address = decode_AM_address<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.C = (value >> 7) & 1;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .ps = new_status,
        };
        MemoryWrite write = {
            .address = address,
            .value = new_value,
        };
        return {result, write};
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::Accumulator>
struct LogicalShiftRightA {
    Result operator()(Processor const &proc) {
        value_t new_value = proc.a >> 1U;
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.C = proc.a & 1;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .a = new_value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::ZeroPage, AddressingType::ZeroPageX, AddressingType::Absolute,
                         AddressingType::AbsoluteX>
struct LogicalShiftRight {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        value_t new_value = value >> 1U;
        address_t address = decode_AM_address<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.C = value & 1;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .ps = new_status,
        };
        MemoryWrite write = {
            .address = address,
            .value = new_value,
        };
        return {result, write};
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::Accumulator>
struct RotateLeftA {
    Result operator()(Processor const &proc) {
        value_t new_value = rol8(proc.a, 1);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.C = (proc.a >> 7) & 1;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .a = new_value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::ZeroPage, AddressingType::ZeroPageX, AddressingType::Absolute,
                         AddressingType::AbsoluteX>
struct RotateLeft {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        value_t new_value = rol8(value, 1);
        address_t address = decode_AM_address<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.C = (value >> 7) & 1;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .ps = new_status,
        };
        MemoryWrite write = {
            .address = address,
            .value = new_value,
        };
        return {result, write};
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::Accumulator>
struct RotateRightA {
    Result operator()(Processor const &proc) {
        value_t new_value = ror8(proc.a, 1);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.C = proc.a & 1;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .a = new_value,
            .ps = new_status,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::ZeroPage, AddressingType::ZeroPageX, AddressingType::Absolute,
                         AddressingType::AbsoluteX>
struct RotateRight {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        value_t value = decode_AM_value<am>(proc, imm);
        value_t new_value = ror8(value, 1);
        address_t address = decode_AM_address<am>(proc, imm);
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.C = value & 1;
        new_status.flag.Z = 0 == new_value;
        new_status.flag.N = (new_value >> 7) & 1;
        ProcessorChange result = {
            .pc = proc.pc + size,
            .ps = new_status,
        };
        MemoryWrite write = {
            .address = address,
            .value = new_value,
        };
        return {result, write};
    }
};

/**
 * Jumps & Calls
 */

template <AddressingMode am, size_t size>
    requires is_one_of_v<am, AddressingType::Absolute, AddressingType::Indirect>
struct Jump {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        address_t address = decode_AM_address<am>(proc, imm);
        ProcessorChange result = {
            .pc = address,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Absolute>
struct JumpSubr {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        address_t address = decode_AM_address<am>(proc, imm);
        ProcessorChange result = {
            .pc = address,
            .sp = proc.sp - 2,
        };
        auto retaddr = proc.pc + size;
        MemoryWrite writelo = {
            .address = to_spaddr(proc.sp - 1),
            .value = static_cast<value_t>(retaddr & 0xFF),
        };
        MemoryWrite writehi = {
            .address = to_spaddr(proc.sp),
            .value = static_cast<value_t>(retaddr >> 8),
        };
        return {result, {writehi, writelo}};
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct ReturnSubr {
    Result operator()(Processor const &proc) {
        auto value = proc.memory.load_address(to_spaddr(proc.sp + 2)).value();
        ProcessorChange result = {
            .pc = value,
            .sp = proc.sp + 2,
        };
        return result;
    }
};

/**
 * Branches
 */

namespace BranchFlag {
struct C {};
struct N {};
struct Z {};
struct V {};
} // namespace BranchFlag

template <typename condition, bool check_set>
constexpr inline bool condition_satisfied(Processor const &proc) {
    if constexpr (std::is_same_v<condition, BranchFlag::C>) {
        return proc.ps.flag.C == check_set;
    } else if constexpr (std::is_same_v<condition, BranchFlag::N>) {
        return proc.ps.flag.N == check_set;
    } else if constexpr (std::is_same_v<condition, BranchFlag::Z>) {
        return proc.ps.flag.Z == check_set;
    } else if constexpr (std::is_same_v<condition, BranchFlag::V>) {
        return proc.ps.flag.V == check_set;
    }
}

template <typename condition, bool check_set, AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Relative>
struct Branch {
    Result operator()(Processor const &proc, typename am::imm_type const &imm) {
        if (condition_satisfied<condition, check_set>(proc)) {
            address_t new_pc = decode_AM_address<am>(proc, imm) + AddressingType::Relative::size;
            ProcessorChange result = {.pc = new_pc};
            return result;
        }
        return ProcessorChange::nop<size>(proc);
    }
};

template <AddressingMode am, size_t size>
struct BranchCarryClear : public Branch<BranchFlag::C, false, am, size> {};
template <AddressingMode am, size_t size>
struct BranchCarrySet : public Branch<BranchFlag::C, true, am, size> {};
template <AddressingMode am, size_t size>
struct BranchEqual : public Branch<BranchFlag::Z, true, am, size> {};
template <AddressingMode am, size_t size>
struct BranchMinus : public Branch<BranchFlag::N, true, am, size> {};
template <AddressingMode am, size_t size>
struct BranchNotEqual : public Branch<BranchFlag::Z, false, am, size> {};
template <AddressingMode am, size_t size>
struct BranchPlus : public Branch<BranchFlag::N, false, am, size> {};
template <AddressingMode am, size_t size>
struct BranchOverflowClear : public Branch<BranchFlag::V, false, am, size> {};
template <AddressingMode am, size_t size>
struct BranchOverflowSet : public Branch<BranchFlag::V, true, am, size> {};

/**
 * Status Flag Changes
 */

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct ClearFlagC {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {
            .pc = proc.pc + size,
        };
        if (proc.ps.flag.C) {
            Processor::processor_status_t new_status = proc.ps;
            new_status.flag.C = 0;
            result.ps = new_status;
        }
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct ClearFlagD {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {
            .pc = proc.pc + size,
        };
        if (proc.ps.flag.D) {
            Processor::processor_status_t new_status = proc.ps;
            new_status.flag.D = 0;
            result.ps = new_status;
        }
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct ClearFlagI {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {
            .pc = proc.pc + size,
        };
        if (proc.ps.flag.I) {
            Processor::processor_status_t new_status = proc.ps;
            new_status.flag.I = 0;
            result.ps = new_status;
        }
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct ClearFlagV {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {
            .pc = proc.pc + size,
        };
        if (proc.ps.flag.V) {
            Processor::processor_status_t new_status = proc.ps;
            new_status.flag.V = 0;
            result.ps = new_status;
        }
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct SetFlagC {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {
            .pc = proc.pc + size,
        };
        if (!proc.ps.flag.C) {
            Processor::processor_status_t new_status = proc.ps;
            new_status.flag.C = 1;
            result.ps = new_status;
        }
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct SetFlagD {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {
            .pc = proc.pc + size,
        };
        if (!proc.ps.flag.D) {
            Processor::processor_status_t new_status = proc.ps;
            new_status.flag.D = 1;
            result.ps = new_status;
        }
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct SetFlagI {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {
            .pc = proc.pc + size,
        };
        if (!proc.ps.flag.I) {
            Processor::processor_status_t new_status = proc.ps;
            new_status.flag.I = 1;
            result.ps = new_status;
        }
        return result;
    }
};

/**
 * System Functions
 */

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct Break {
    Result operator()(Processor const &proc) {
        // Hard-coded
        auto handler = proc.memory.load_address(0xFFFEU).value();
        Processor::processor_status_t new_status = proc.ps;
        new_status.flag.B = 1;
        // TODO: Multiple writes
        MemoryWrite save_pc_lo = {.address = to_spaddr(proc.sp - 2), .value = static_cast<value_t>(proc.pc & 0xFFU)};
        MemoryWrite save_pc_hi = {.address = to_spaddr(proc.sp - 1), .value = static_cast<value_t>(proc.pc >> 8U)};
        MemoryWrite save_ps = {.address = to_spaddr(proc.sp), .value = proc.ps.regr};
        ProcessorChange result = {
            .pc = handler,
            .sp = proc.sp - 3,
            .ps = new_status,
        };
        return {result, {save_pc_hi, save_pc_lo, save_ps}};
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct Nop {
    Result operator()(Processor const &proc) {
        ProcessorChange result = {
            .pc = proc.pc + size,
        };
        return result;
    }
};

template <AddressingMode am, size_t size>
    requires std::is_same_v<am, AddressingType::Implied>
struct ReturnIntr {
    Result operator()(Processor const &proc) {
        Processor::processor_status_t new_status;
        new_status.regr = proc.memory.load_address(to_spaddr(proc.sp + 1)).value();
        address_t new_pc = proc.memory.load_address(to_spaddr(proc.sp + 3)).value();
        ProcessorChange result = {
            .pc = new_pc,
            .sp = proc.sp + 3,
            .ps = new_status,
        };
        return result;
    }
};

template <typename F, typename... Args>
// requires requires(F &&fn, Processor const &proc, Args &&...args) { fn(proc, args...); }
    requires std::invocable<F, Processor const &, Args...>
inline auto i_(F &&fn, Args &&...args) {
    return std::bind(fn, std::placeholders::_1, std::forward<Args>(args)...);
}

Processor &operator<<(Processor &proc, std::function<Instruction::Result(Processor const &)> lazy_instruction) {
    if (lazy_instruction) {
        auto change = lazy_instruction(proc);
        change.m_pchange.commit_to(proc);
        change.m_write.commit_to(proc);
    }
    return proc;
}

struct ByteInstruction : std::function<Result(const Processor &)> {
    char metadata_mne[4] = {0};
    // Param::Params metadata_params;

    ByteInstruction &add_mne(std::string_view name) {
        std::memcpy(metadata_mne, name.data(), 3);
        // metadata_params = params;
        return *this;
    }
};

namespace Assemble {

using namespace std::literals;

struct AsmInst {
    Param::Params parameter;
    // Last byte should be null
    char mne[4];

    AsmInst(std::string_view rawmne, Param::Params &&param) : parameter{param} {
        std::memcpy(mne, rawmne.data(), 3);
        mne[3] = '\0';
    }
};

AsmInst parse_instruction(std::string_view line) {
    constexpr auto const split_comment = [](std::string_view line) -> auto {
        auto index = line.find_first_of(';');
        return index == std::string_view::npos ? std::make_pair(line, ""sv)
                                               : std::make_pair(line.substr(0, index), line.substr(index));
    };
    constexpr auto const split_instruction = [](std::string_view line) -> auto {
        auto stage1 = trim_left(trim_right(line));
        auto index = stage1.find_first_of(' ');
        return index == std::string_view::npos
                   ? std::make_pair(stage1, ""sv)
                   : std::make_pair(stage1.substr(0, index), trim_left(stage1.substr(index)));
    };

    auto [raw_instruction, _] = split_comment(line);
    auto [mne, raw_parameters] = split_instruction(raw_instruction);
    return {mne, Param::parse(raw_parameters)};
}
// template <typename Function, AddressingMode am, size_t size>
// concept valid_fun1 = requires(Processor const &p, am::imm_size imm) { Function<typename am, size>{}(p, imm); };
// template <typename Function>
// concept valid_fun0 = requires(Processor const &p) { Function<typename am, size>{}(p); };

// template <template <AddressingMode,size_t> typename Function, AddressingMode am, size_t size>
// concept valid_fun = requires() { typename Function<am, size>; };

template <size_t argc, TpString op, AddressingMode am, size_t size = am::size>
struct to_impl {
    template <TpString, template <AddressingMode, size_t> typename>
    struct match {
        static constexpr bool value = false;
    };
    template <TpString mne, template <AddressingMode, size_t> typename Function>
        requires requires { typename Function<am, size>; }
    struct match<mne, Function> {
        static constexpr bool value = tpstr_equal_v<op, mne>;
        using Fun = Function<am, size>;
    };

    struct no_match {
        static constexpr bool value = false;
    };

    using function = std::conditional_t<
        bool(0 == argc),
        std::disjunction<
            match<"LSR", LogicalShiftRightA>, match<"ASL", ArithShiftLeftA>, match<"BRK", Break>,
            match<"CLC", ClearFlagC>, match<"CLD", ClearFlagD>, match<"CLI", ClearFlagI>, match<"CLV", ClearFlagV>,
            match<"DEC", DecrementMemory>, match<"DEX", DecrementX>, match<"DEY", DecrementY>, match<"EOR", LogicalEOR>,
            match<"INC", IncrementMemory>, match<"INX", IncrementX>, match<"INY", IncrementY>,
            match<"LSR", LogicalShiftRightA>, match<"NOP", Nop>, match<"PHA", PushA>, match<"PHP", PushPs>,
            match<"PLA", PullA>, match<"PLP", PullPs>, match<"ROL", RotateLeftA>, match<"ROR", RotateRightA>,
            match<"RTI", ReturnIntr>, match<"RTS", ReturnSubr>, match<"SEC", SetFlagC>, match<"SED", SetFlagD>,
            match<"SEI", SetFlagI>, match<"TAX", TransferAX>, match<"TAY", TransferAY>, match<"TSX", TransferSpX>,
            match<"TXA", TransferXA>, match<"TXS", TransferXSp>, match<"TYA", TransferYA>>,
        std::conditional_t<
            bool(1 == argc),
            std::disjunction<
                match<"ADC", AddWithCarry>, match<"LDA", LoadA>, match<"ADC", AddWithCarry>, match<"AND", LogicalAND>,
                match<"ASL", ArithShiftLeft>, match<"BCC", BranchCarryClear>, match<"BCS", BranchCarrySet>,
                match<"BEQ", BranchEqual>, match<"BIT", LogicalBitTest>, match<"BMI", BranchMinus>,
                match<"BNE", BranchNotEqual>, match<"BPL", BranchPlus>, match<"BRK", Break>,
                match<"BVC", BranchOverflowClear>, match<"BVS", BranchOverflowSet>, match<"CLC", ClearFlagC>,
                match<"CLD", ClearFlagD>, match<"CLI", ClearFlagI>, match<"CLV", ClearFlagV>, match<"CMP", CompareA>,
                match<"CPX", CompareX>, match<"CPY", CompareY>, match<"DEC", DecrementMemory>, match<"DEX", DecrementX>,
                match<"DEY", DecrementY>, match<"EOR", LogicalEOR>, match<"INC", IncrementMemory>,
                match<"INX", IncrementX>, match<"INY", IncrementY>, match<"JMP", Jump>, match<"JSR", JumpSubr>,
                match<"LDA", LoadA>, match<"LDX", LoadX>, match<"LDY", LoadY>, match<"LSR", LogicalShiftRight>,
                match<"NOP", Nop>, match<"ORA", LogicalOR>, match<"PHA", PushA>, match<"PHP", PushPs>,
                match<"PLA", PullA>, match<"PLP", PullPs>, match<"ROL", RotateLeft>, match<"ROR", RotateRight>,
                match<"SBC", SubWithCarry>, match<"STA", StoreA>, match<"STX", StoreX>, match<"STY", StoreY>>,
            no_match>>;
};

template <AddressingMode am, size_t size = am::size, typename T>
inline auto to_inst1(std::string_view mne, T &&arg) {
#define try_match(exp_mne, fn)                                                                                         \
    if constexpr (requires(const Processor &p) { fn{}(p, std::forward<T>(arg)); })                                     \
        if (str_equal<exp_mne>(mne))                                                                                   \
            return ByteInstruction{std::bind(fn{}, std::placeholders::_1, std::forward<T>(arg))}.add_mne(mne);

    try_match("ADC", (AddWithCarry<am, size>));
    try_match("LDA", (LoadA<am, size>));
    try_match("ADC", (AddWithCarry<am, size>));
    try_match("AND", (LogicalAND<am, size>));
    try_match("ASL", (ArithShiftLeft<am, size>));
    try_match("BCC", (Branch<BranchFlag::C, false, am, size>));
    try_match("BCS", (Branch<BranchFlag::C, true, am, size>));
    try_match("BEQ", (Branch<BranchFlag::Z, true, am, size>));
    try_match("BIT", (LogicalBitTest<am, size>));
    try_match("BMI", (Branch<BranchFlag::N, true, am, size>));
    try_match("BNE", (Branch<BranchFlag::Z, false, am, size>));
    try_match("BPL", (Branch<BranchFlag::N, false, am, size>));
    try_match("BRK", (Break<am, size>));
    try_match("BVC", (Branch<BranchFlag::V, false, am, size>));
    try_match("BVS", (Branch<BranchFlag::V, true, am, size>));
    try_match("CLC", (ClearFlagC<am, size>));
    try_match("CLD", (ClearFlagD<am, size>));
    try_match("CLI", (ClearFlagI<am, size>));
    try_match("CLV", (ClearFlagV<am, size>));
    try_match("CMP", (CompareA<am, size>));
    try_match("CPX", (CompareX<am, size>));
    try_match("CPY", (CompareY<am, size>));
    try_match("DEC", (DecrementMemory<am, size>));
    try_match("DEX", (DecrementX<am, size>));
    try_match("DEY", (DecrementY<am, size>));
    try_match("EOR", (LogicalEOR<am, size>));
    try_match("INC", (IncrementMemory<am, size>));
    try_match("INX", (IncrementX<am, size>));
    try_match("INY", (IncrementY<am, size>));
    try_match("JMP", (Jump<am, size>));
    try_match("JSR", (JumpSubr<am, size>));
    try_match("LDA", (LoadA<am, size>));
    try_match("LDX", (LoadX<am, size>));
    try_match("LDY", (LoadY<am, size>));
    try_match("LSR", (LogicalShiftRight<am, size>));
    try_match("NOP", (Nop<am, size>));
    try_match("ORA", (LogicalOR<am, size>));
    try_match("PHA", (PushA<am, size>));
    try_match("PHP", (PushPs<am, size>));
    try_match("PLA", (PullA<am, size>));
    try_match("PLP", (PullPs<am, size>));
    try_match("ROL", (RotateLeft<am, size>));
    try_match("ROR", (RotateRight<am, size>));
    try_match("SBC", (SubWithCarry<am, size>));
    try_match("STA", (StoreA<am, size>));
    try_match("STX", (StoreX<am, size>));
    try_match("STY", (StoreY<am, size>));
#undef try_match
    return ByteInstruction{nullptr};
}
template <AddressingMode am, size_t size = am::size>
inline auto to_inst0(std::string_view mne) {

#define try_match(exp_mne, fn)                                                                                         \
    if constexpr (requires(const Processor &p) { fn{}(p); })                                                           \
        if (str_equal<exp_mne>(mne))                                                                                   \
            return ByteInstruction{fn{}}.add_mne(mne);

    try_match("LSR", (LogicalShiftRightA<am, size>));
    try_match("ASL", (ArithShiftLeftA<am, size>));
    try_match("BRK", (Break<am, size>));
    try_match("CLC", (ClearFlagC<am, size>));
    try_match("CLD", (ClearFlagD<am, size>));
    try_match("CLI", (ClearFlagI<am, size>));
    try_match("CLV", (ClearFlagV<am, size>));
    try_match("DEC", (DecrementMemory<am, size>));
    try_match("DEX", (DecrementX<am, size>));
    try_match("DEY", (DecrementY<am, size>));
    try_match("EOR", (LogicalEOR<am, size>));
    try_match("INC", (IncrementMemory<am, size>));
    try_match("INX", (IncrementX<am, size>));
    try_match("INY", (IncrementY<am, size>));
    try_match("LSR", (LogicalShiftRightA<am, size>));
    try_match("NOP", (Nop<am, size>));
    try_match("PHA", (PushA<am, size>));
    try_match("PHP", (PushPs<am, size>));
    try_match("PLA", (PullA<am, size>));
    try_match("PLP", (PullPs<am, size>));
    try_match("ROL", (RotateLeftA<am, size>));
    try_match("ROR", (RotateRightA<am, size>));
    try_match("RTI", (ReturnIntr<am, size>));
    try_match("RTS", (ReturnSubr<am, size>));
    try_match("SEC", (SetFlagC<am, size>));
    try_match("SED", (SetFlagD<am, size>));
    try_match("SEI", (SetFlagI<am, size>));
    try_match("TAX", (TransferAX<am, size>));
    try_match("TAY", (TransferAY<am, size>));
    try_match("TSX", (TransferSpX<am, size>));
    try_match("TXA", (TransferXA<am, size>));
    try_match("TXS", (TransferXSp<am, size>));
    try_match("TYA", (TransferYA<am, size>));
#undef try_match
    return ByteInstruction{nullptr};
}

using LabelMap = std::map<std::string, address_t>;
using LabelInstructionMap = std::map<size_t, std::string>;

template <std::ranges::range R>
// requires std::is_same_v<AsmInst, std::ranges::range_value_t<R>>
auto jit_listing(R &&listing, LabelMap const &labels, address_t base) {
    std::vector<ByteInstruction> jited;
    for (auto i : listing) {
        auto &mne = i.mne;
        auto ins = std::visit(
            overloads{
                [mne](Param::A p) { return to_inst0<AddressingType::Accumulator>(mne); },
                [mne](Param::Imm p) { return to_inst1<AddressingType::Immediate>(mne, p.store); },
                [mne](Param::Address p) {
                    // Check if Zero
                    auto &addr = p.store;
                    if (is_representable<value_t>(addr)) {
                        return to_inst1<AddressingType::ZeroPage>(mne, addr);
                    } else {
                        return to_inst1<AddressingType::Absolute>(mne, addr);
                    }
                },
                [mne, &labels, base](Param::Label p) {
                    std::string search_key{p.store};
                    auto addr = labels.contains(search_key) ? std::make_optional<address_t>(labels.at(search_key))
                                                            : std::nullopt;
                    if (str_inlist<"BCC", "BCS", "BEQ", "BMI", "BNE", "BPL", "BVC", "BVS">(mne)) {
                        // TODO: Offset is not properly calculated
                        return ByteInstruction{nullptr};
                        // If we do not know, we will set offset to 0
                        address_diff_t offset = addr ? 0U : addr.value() - (base);
                        value_t store_offset = is_representable<value_t>(offset)
                                                   ? static_cast<value_t>(static_cast<svalue_t>(offset))
                                                   : 0U;
                        return to_inst1<AddressingType::Relative>(mne, store_offset);
                    }
                    auto abs_addr = addr.value_or(0U);
                    if (is_representable<value_t>(abs_addr)) {
                        return to_inst1<AddressingType::ZeroPage>(mne, abs_addr);
                    } else {
                        return to_inst1<AddressingType::Absolute>(mne, abs_addr);
                    }
                },
                //  [](Param::Offset p) {},
                [mne](Param::Implied p) { return to_inst0<AddressingType::Implied>(mne); },
                [mne](Param::AddressX p) {
                    // Check if Zero
                    auto &addr = p.first.store;
                    if (is_representable<value_t>(addr)) {
                        return to_inst1<AddressingType::ZeroPageX>(mne, addr);
                    } else {
                        return to_inst1<AddressingType::AbsoluteX>(mne, addr);
                    }
                },
                // [](Param::LabelX p) {},
                [mne](Param::AddressY p) {
                    // Check if Zero
                    auto &addr = p.first.store;
                    if (is_representable<value_t>(addr)) {
                        return to_inst1<AddressingType::ZeroPageY>(mne, addr);
                    } else {
                        return to_inst1<AddressingType::AbsoluteY>(mne, addr);
                    }
                },
                // [](Param::LabelY p) {},
                [mne](Param::IndirectAddress p) { return to_inst1<AddressingType::Indirect>(mne, p.second.store); },
                // [](Param::IndirectLabel p) {},
                [mne](Param::IdxIndirectAddress p) { return to_inst1<AddressingType::IndirectX>(mne, p.second.store); },
                // [](std::tuple<Param::Indirect, Param::Label, Param::X> p) {},
                [mne](Param::IndirectIdxAddress p) { return to_inst1<AddressingType::IndirectY>(mne, p.second.store); },
                // [](std::tuple<Param::Indirect, Param::Label, Param::Y> p) {},
                //    [](std::monostate) { /* Error! */ },
                [](auto x) {
#ifndef NDEBUG
                    std::cerr << std::format("[Internal] Something is wrong when assembling with: {}\n",
                                             typeid(x).name());
#endif
                    return ByteInstruction{nullptr};
                }},
            i.parameter);
        jited.push_back(ins);
    }
    return jited;
}

template <std::ranges::range R>
auto prescan_label(R &&listing, LabelInstructionMap const &listing_labels, address_t base, LabelMap const &known) {
    LabelMap result = known;
    auto current_addr{base};
    for (auto [index, i] : listing | std::views::enumerate) {
        auto &mne = i.mne;
        if (listing_labels.contains(index)) {
            result.insert_or_assign(listing_labels.at(index), current_addr);
        }
        std::visit(overloads{[&](Param::A) { current_addr += AddressingType::Accumulator::size; },
                             [&](Param::Imm) { current_addr += AddressingType::Immediate::size; },
                             [&](Param::Address p) {
                                 if (is_representable<value_t>(p.store)) {
                                     current_addr += AddressingType::ZeroPage::size;
                                 } else {
                                     current_addr += AddressingType::Absolute::size;
                                 }
                             },
                             [&](Param::Label p) {
                                 std::string search_key{p.store};
                                 if (known.contains(search_key) && is_representable<value_t>(known.at(search_key))) {
                                     current_addr += AddressingType::ZeroPage::size;
                                 } else {
                                     current_addr += AddressingType::Absolute::size;
                                 }
                             },
                             [&](Param::Offset) { current_addr += AddressingType::Relative::size; },
                             [&](Param::Implied) { current_addr += AddressingType::Implied::size; },
                             [&](Param::AddressX p) {
                                 if (is_representable<value_t>(p.first.store)) {
                                     current_addr += AddressingType::ZeroPageX::size;
                                 } else {
                                     current_addr += AddressingType::AbsoluteX::size;
                                 }
                             },
                             [&](Param::LabelX p) {
                                 std::string search_key{p.first.store};
                                 if (known.contains(search_key) && is_representable<value_t>(known.at(search_key))) {
                                     current_addr += AddressingType::ZeroPageX::size;
                                 } else {
                                     current_addr += AddressingType::AbsoluteX::size;
                                 }
                             },
                             [&](Param::AddressY p) {
                                 if (is_representable<value_t>(p.first.store)) {
                                     current_addr += AddressingType::ZeroPageY::size;
                                 } else {
                                     current_addr += AddressingType::AbsoluteY::size;
                                 }
                             },
                             [&](Param::LabelY p) {
                                 std::string search_key{p.first.store};
                                 if (known.contains(search_key) && is_representable<value_t>(known.at(search_key))) {
                                     current_addr += AddressingType::ZeroPageY::size;
                                 } else {
                                     current_addr += AddressingType::AbsoluteY::size;
                                 }
                             },
                             [&](Param::IndirectAddress) { current_addr += AddressingType::Indirect::size; },
                             [&](Param::IndirectLabel) { current_addr += AddressingType::Indirect::size; },
                             [&](Param::IdxIndirectAddress) { current_addr += AddressingType::IndirectX::size; },
                             [&](Param::IdxIndirectLabel) { current_addr += AddressingType::IndirectX::size; },
                             [&](Param::IndirectIdxAddress) { current_addr += AddressingType::IndirectY::size; },
                             [&](Param::IndirectIdxLabel) { current_addr += AddressingType::IndirectY::size; },
                             //    [](std::monostate) { /* Error! */ },
                             [](auto x) {
#ifndef NDEBUG
                                 std::cerr << std::format(
                                     "[Internal] Something is wrong when scanning label with: {}\n", typeid(x).name());
#endif
                             }},
                   i.parameter);
    }
    return result;
}

auto parse_text(std::string_view file_content) {
    std::vector<AsmInst> listing;
    LabelInstructionMap listing_label;
    for (auto line : file_content | std::views::split('\n') | std::views::filter(not_empty) | cast_sv) {
        bool parsed = false;
        auto label_ending = line.find_first_of(':');
        auto next = line;
        if (label_ending != std::string_view::npos) {
            listing_label.emplace(listing.size(), line.substr(0, label_ending));
            next = trim_left(line.substr(label_ending + 1));
            parsed = true;
        }
        if (next.find_first_not_of(' ') != std::string_view::npos) {
            listing.push_back(parse_instruction(next));
            parsed = true;
        }
#ifndef NDEBUG
        if (!parsed) {
            std::cerr << std::format("[Internal] Cannot parse, ignoring: `{}`\n", line);
        }
#endif
    }
    size_t const base{0x200};
    std::map<std::string, address_t> labels = prescan_label(listing, listing_label, base, {});
    return std::make_tuple(jit_listing(listing, labels, base), listing, labels);
}

template <AddressingMode am>
constexpr inline value_t encode_inst_op(std::string_view mne) {
#define try_match(value, exp_mne, exp_am)                                                                              \
    if constexpr (std::is_same_v<exp_am, am>)                                                                          \
        if (str_equal<exp_mne>(mne))                                                                                   \
            return (value);

    try_match(0x00, "BRK", AddressingType::Implied);
    try_match(0x01, "ORA", AddressingType::IndirectX);
    try_match(0x05, "ORA", AddressingType::ZeroPage);
    try_match(0x06, "ASL", AddressingType::ZeroPage);
    try_match(0x08, "PHP", AddressingType::Implied);
    try_match(0x09, "ORA", AddressingType::Immediate);
    try_match(0x0A, "ASL", AddressingType::Accumulator);
    try_match(0x0D, "ORA", AddressingType::Absolute);
    try_match(0x0E, "ASL", AddressingType::Absolute);
    try_match(0x10, "BPL", AddressingType::Relative);
    try_match(0x11, "ORA", AddressingType::IndirectY);
    try_match(0x15, "ORA", AddressingType::ZeroPageX);
    try_match(0x16, "ASL", AddressingType::ZeroPageX);
    try_match(0x18, "CLC", AddressingType::Implied);
    try_match(0x19, "ORA", AddressingType::AbsoluteY);
    try_match(0x1D, "ORA", AddressingType::AbsoluteX);
    try_match(0x1E, "ASL", AddressingType::AbsoluteX);
    try_match(0x20, "JSR", AddressingType::Absolute);
    try_match(0x21, "AND", AddressingType::IndirectX);
    try_match(0x24, "BIT", AddressingType::ZeroPage);
    try_match(0x25, "AND", AddressingType::ZeroPage);
    try_match(0x26, "ROL", AddressingType::ZeroPage);
    try_match(0x28, "PLP", AddressingType::Implied);
    try_match(0x29, "AND", AddressingType::Immediate);
    try_match(0x2A, "ROL", AddressingType::Accumulator);
    try_match(0x2C, "BIT", AddressingType::Absolute);
    try_match(0x2D, "AND", AddressingType::Absolute);
    try_match(0x2E, "ROL", AddressingType::Absolute);
    try_match(0x30, "BMI", AddressingType::Relative);
    try_match(0x31, "AND", AddressingType::IndirectY);
    try_match(0x35, "AND", AddressingType::ZeroPageX);
    try_match(0x36, "ROL", AddressingType::ZeroPageX);
    try_match(0x38, "SEC", AddressingType::Implied);
    try_match(0x39, "AND", AddressingType::AbsoluteY);
    try_match(0x3D, "AND", AddressingType::AbsoluteX);
    try_match(0x3E, "ROL", AddressingType::AbsoluteX);
    try_match(0x40, "RTI", AddressingType::Implied);
    try_match(0x41, "EOR", AddressingType::IndirectX);
    try_match(0x45, "EOR", AddressingType::ZeroPage);
    try_match(0x46, "LSR", AddressingType::ZeroPage);
    try_match(0x48, "PHA", AddressingType::Implied);
    try_match(0x49, "EOR", AddressingType::Immediate);
    try_match(0x4A, "LSR", AddressingType::Accumulator);
    try_match(0x4C, "JMP", AddressingType::Absolute);
    try_match(0x4D, "EOR", AddressingType::Absolute);
    try_match(0x4E, "LSR", AddressingType::Absolute);
    try_match(0x50, "BVC", AddressingType::Relative);
    try_match(0x51, "EOR", AddressingType::IndirectY);
    try_match(0x55, "EOR", AddressingType::ZeroPageX);
    try_match(0x56, "LSR", AddressingType::ZeroPageX);
    try_match(0x58, "CLI", AddressingType::Implied);
    try_match(0x59, "EOR", AddressingType::AbsoluteY);
    try_match(0x5D, "EOR", AddressingType::AbsoluteX);
    try_match(0x5E, "LSR", AddressingType::AbsoluteX);
    try_match(0x60, "RTS", AddressingType::Implied);
    try_match(0x61, "ADC", AddressingType::IndirectX);
    try_match(0x65, "ADC", AddressingType::ZeroPage);
    try_match(0x66, "ROR", AddressingType::ZeroPage);
    try_match(0x68, "PLA", AddressingType::Implied);
    try_match(0x69, "ADC", AddressingType::Immediate);
    try_match(0x6A, "ROR", AddressingType::Accumulator);
    try_match(0x6C, "JMP", AddressingType::Absolute);
    try_match(0x6D, "ADC", AddressingType::Absolute);
    try_match(0x6E, "ROR", AddressingType::Absolute);
    try_match(0x70, "BVC", AddressingType::Relative);
    try_match(0x71, "ADC", AddressingType::IndirectY);
    try_match(0x75, "ADC", AddressingType::ZeroPageX);
    try_match(0x76, "ROR", AddressingType::ZeroPageX);
    try_match(0x78, "SEI", AddressingType::Implied);
    try_match(0x79, "ADC", AddressingType::AbsoluteY);
    try_match(0x7D, "ADC", AddressingType::AbsoluteX);
    try_match(0x7E, "ROR", AddressingType::AbsoluteX);
    try_match(0x81, "STA", AddressingType::IndirectX);
    try_match(0x84, "STY", AddressingType::ZeroPage);
    try_match(0x85, "STA", AddressingType::ZeroPage);
    try_match(0x86, "STX", AddressingType::ZeroPage);
    try_match(0x88, "DEY", AddressingType::Implied);
    try_match(0x8A, "TXA", AddressingType::Implied);
    try_match(0x8C, "STY", AddressingType::Absolute);
    try_match(0x8D, "STA", AddressingType::Absolute);
    try_match(0x8E, "STX", AddressingType::Absolute);
    try_match(0x90, "BCC", AddressingType::Relative);
    try_match(0x91, "STA", AddressingType::IndirectY);
    try_match(0x94, "STY", AddressingType::ZeroPageX);
    try_match(0x95, "STA", AddressingType::ZeroPageX);
    try_match(0x96, "STX", AddressingType::ZeroPageY);
    try_match(0x98, "TYA", AddressingType::Implied);
    try_match(0x99, "STA", AddressingType::AbsoluteY);
    try_match(0x9D, "STA", AddressingType::AbsoluteX);
    try_match(0xA0, "LDY", AddressingType::Immediate);
    try_match(0xA1, "LDA", AddressingType::IndirectX);
    try_match(0xA2, "LDX", AddressingType::Immediate);
    try_match(0xA4, "LDY", AddressingType::ZeroPage);
    try_match(0xA5, "LDA", AddressingType::ZeroPage);
    try_match(0xA6, "LDX", AddressingType::ZeroPage);
    try_match(0xA8, "TAY", AddressingType::Implied);
    try_match(0xA9, "LDA", AddressingType::Immediate);
    try_match(0xAA, "LDX", AddressingType::Accumulator);
    try_match(0xAC, "LDY", AddressingType::Absolute);
    try_match(0xAD, "LDA", AddressingType::Absolute);
    try_match(0xAE, "LDX", AddressingType::Absolute);
    try_match(0xB0, "BCS", AddressingType::Relative);
    try_match(0xB1, "LDA", AddressingType::IndirectY);
    try_match(0xB4, "LDY", AddressingType::ZeroPageX);
    try_match(0xB5, "LDA", AddressingType::ZeroPageX);
    try_match(0xB6, "LDX", AddressingType::ZeroPageY);
    try_match(0xB8, "CLV", AddressingType::Implied);
    try_match(0xB9, "LDA", AddressingType::AbsoluteY);
    try_match(0xBA, "TSX", AddressingType::Implied);
    try_match(0xBC, "LDY", AddressingType::AbsoluteX);
    try_match(0xBD, "LDA", AddressingType::AbsoluteX);
    try_match(0xBE, "LDX", AddressingType::AbsoluteY);
    try_match(0xC0, "CPY", AddressingType::Immediate);
    try_match(0xC1, "CMP", AddressingType::IndirectX);
    try_match(0xC4, "CPY", AddressingType::ZeroPage);
    try_match(0xC5, "CMP", AddressingType::ZeroPage);
    try_match(0xC6, "DEC", AddressingType::ZeroPage);
    try_match(0xC8, "INY", AddressingType::Implied);
    try_match(0xC9, "CMP", AddressingType::Immediate);
    try_match(0xCA, "DEX", AddressingType::Implied);
    try_match(0xCC, "CPY", AddressingType::Absolute);
    try_match(0xCD, "CMP", AddressingType::Absolute);
    try_match(0xCE, "DEC", AddressingType::Absolute);
    try_match(0xD0, "BNE", AddressingType::Relative);
    try_match(0xD1, "CMP", AddressingType::IndirectY);
    try_match(0xD5, "CMP", AddressingType::ZeroPageX);
    try_match(0xD6, "DEC", AddressingType::ZeroPageY);
    try_match(0xD8, "CLD", AddressingType::Implied);
    try_match(0xD9, "CMP", AddressingType::AbsoluteY);
    try_match(0xDD, "CMP", AddressingType::AbsoluteX);
    try_match(0xDE, "DEC", AddressingType::AbsoluteY);
    try_match(0xE0, "CPX", AddressingType::Immediate);
    try_match(0xE1, "SBC", AddressingType::IndirectX);
    try_match(0xE4, "CPX", AddressingType::ZeroPage);
    try_match(0xE5, "SBC", AddressingType::ZeroPage);
    try_match(0xE6, "INC", AddressingType::ZeroPage);
    try_match(0xE8, "INX", AddressingType::Implied);
    try_match(0xE9, "SBC", AddressingType::Immediate);
    try_match(0xEA, "NOP", AddressingType::Implied);
    try_match(0xEC, "CPX", AddressingType::Absolute);
    try_match(0xED, "SBC", AddressingType::Absolute);
    try_match(0xEE, "INC", AddressingType::Absolute);
    try_match(0xF0, "BNE", AddressingType::Relative);
    try_match(0xF1, "SBC", AddressingType::IndirectY);
    try_match(0xF5, "SBC", AddressingType::ZeroPageX);
    try_match(0xF6, "INC", AddressingType::ZeroPageY);
    try_match(0xF8, "SED", AddressingType::Implied);
    try_match(0xF9, "SBC", AddressingType::AbsoluteY);
    try_match(0xFD, "SBC", AddressingType::AbsoluteX);
    try_match(0xFE, "INC", AddressingType::AbsoluteY);
#undef try_match
    return 0x00; // BRK
}

inline auto destruct_hilo(address_t a) {
    return std::make_pair<value_t, value_t>(a >> 8, a & 0xFFU);
}

template <std::ranges::range R>
// requires std::is_same_v<AsmInst, std::ranges::range_value_t<R>>
auto assemble(R &&listing, LabelMap const &labels, address_t base) {
    std::vector<value_t> bytecode;
    for (auto i : listing) {
        auto mne = std::string_view{i.mne};
        std::visit(
            overloads{
                [&bytecode, mne](Param::A) { bytecode.push_back(encode_inst_op<AddressingType::Accumulator>(mne)); },
                [&bytecode, mne](Param::Imm p) {
                    bytecode.append_range(
                        std::vector<value_t>{encode_inst_op<AddressingType::Immediate>(mne), p.store});
                },
                [&bytecode, mne](Param::Address p) {
                    auto &addr = p.store;
                    if (is_representable<value_t>(addr)) {
                        bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::ZeroPage>(mne),
                                                                   static_cast<value_t>(addr)});
                    } else {
                        auto [hi, lo] = destruct_hilo(addr);
                        bytecode.append_range(
                            std::vector<value_t>{encode_inst_op<AddressingType::Absolute>(mne), lo, hi});
                    }
                },
                [&bytecode, mne, &labels, base](Param::Label p) {
                    // TODO: Support relative label branch via checking B
                    std::string search_key{p.store};
                    auto addr = labels.contains(search_key) ? std::make_optional<address_t>(labels.at(search_key))
                                                            : std::nullopt;
                    if (str_inlist<"BCC", "BCS", "BEQ", "BMI", "BNE", "BPL", "BVC", "BVS">(mne)) {
                        // If we do not know, we will set offset to 0
                        address_diff_t offset =
                            addr ? addr.value() - (base + bytecode.size() + AddressingType::Relative::size) : 0;
                        value_t store_offset = static_cast<value_t>(static_cast<svalue_t>(offset));
                        bytecode.append_range(
                            std::vector<value_t>{encode_inst_op<AddressingType::Relative>(mne), store_offset});
                        return;
                    }
                    auto abs_addr = addr.value_or(0U);
                    if (is_representable<value_t>(abs_addr)) {
                        bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::ZeroPage>(mne),
                                                                   static_cast<value_t>(abs_addr)});
                    } else {
                        auto [hi, lo] = destruct_hilo(abs_addr);
                        bytecode.append_range(
                            std::vector<value_t>{encode_inst_op<AddressingType::Absolute>(mne), lo, hi});
                    }
                },
                [&bytecode, mne](Param::Offset p) {
                    bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::Relative>(mne),
                                                               static_cast<value_t>(p.store)});
                },
                [&bytecode, mne](Param::Implied) {
                    return bytecode.push_back(encode_inst_op<AddressingType::Implied>(mne));
                },
                [&bytecode, mne](Param::AddressX p) {
                    auto &addr = p.first.store;
                    if (is_representable<value_t>(addr)) {
                        bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::ZeroPageX>(mne),
                                                                   static_cast<value_t>(addr)});
                    } else {
                        auto [hi, lo] = destruct_hilo(addr);
                        bytecode.append_range(
                            std::vector<value_t>{encode_inst_op<AddressingType::AbsoluteX>(mne), lo, hi});
                    }
                },
                [&bytecode, mne, &labels](Param::LabelX p) {
                    std::string search_key{p.first.store};
                    auto addr = labels.contains(search_key) ? labels.at(search_key) : 0U;
                    if (is_representable<value_t>(addr)) {
                        bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::ZeroPageX>(mne),
                                                                   static_cast<value_t>(addr)});
                    } else {
                        auto [hi, lo] = destruct_hilo(addr);
                        bytecode.append_range(
                            std::vector<value_t>{encode_inst_op<AddressingType::AbsoluteX>(mne), lo, hi});
                    }
                },
                [&bytecode, mne](Param::AddressY p) {
                    auto &addr = p.first.store;
                    if (is_representable<value_t>(addr)) {
                        bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::ZeroPageY>(mne),
                                                                   static_cast<value_t>(addr)});
                    } else {
                        auto [hi, lo] = destruct_hilo(addr);
                        bytecode.append_range(
                            std::vector<value_t>{encode_inst_op<AddressingType::AbsoluteY>(mne), lo, hi});
                    }
                },
                [&bytecode, mne, &labels](Param::LabelY p) {
                    std::string search_key{p.first.store};
                    auto addr = labels.contains(search_key) ? labels.at(search_key) : 0U;
                    if (is_representable<value_t>(addr)) {
                        bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::ZeroPageY>(mne),
                                                                   static_cast<value_t>(addr)});
                    } else {
                        auto [hi, lo] = destruct_hilo(addr);
                        bytecode.append_range(
                            std::vector<value_t>{encode_inst_op<AddressingType::AbsoluteY>(mne), lo, hi});
                    }
                },
                [&bytecode, mne](Param::IndirectAddress p) {
                    auto [hi, lo] = destruct_hilo(p.second.store);
                    bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::Indirect>(mne), lo, hi});
                },
                [&bytecode, mne, &labels](Param::IndirectLabel p) {
                    std::string search_key{p.second.store};
                    auto addr = labels.contains(search_key) ? labels.at(search_key) : 0U;
                    auto [hi, lo] = destruct_hilo(addr);
                    bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::Indirect>(mne), lo, hi});
                },
                [&bytecode, mne](Param::IdxIndirectAddress p) {
                    auto [hi, lo] = destruct_hilo(p.second.store);
                    bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::IndirectX>(mne), lo, hi});
                },
                [&bytecode, mne, &labels](Param::IdxIndirectLabel p) {
                    std::string search_key{p.second.store};
                    auto addr = labels.contains(search_key) ? labels.at(search_key) : 0U;
                    auto [hi, lo] = destruct_hilo(addr);
                    bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::IndirectX>(mne), lo, hi});
                },
                [&bytecode, mne](Param::IndirectIdxAddress p) {
                    auto [hi, lo] = destruct_hilo(p.second.store);
                    bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::IndirectY>(mne), lo, hi});
                },
                [&bytecode, mne, &labels](Param::IndirectIdxLabel p) {
                    std::string search_key{p.second.store};
                    auto addr = labels.contains(search_key) ? labels.at(search_key) : 0U;
                    auto [hi, lo] = destruct_hilo(addr);
                    bytecode.append_range(std::vector<value_t>{encode_inst_op<AddressingType::IndirectY>(mne), lo, hi});
                },
                //    [](std::monostate) { /* Error! */ },
                [](auto x) {
#ifndef NDEBUG
                    std::cerr << std::format("[Internal] Something is wrong when assembling with: {}\n",
                                             typeid(x).name());
#endif
                }},
            i.parameter);
    }
    return bytecode;
}

template <std::ranges::range R>
bool load_to_memory(Processor &proc, address_t address, R &&r) {
    for (auto [offset, b] : r | std::views::enumerate) {
        proc.memory.set_byte(address + offset, b);
    }
    return true;
}

} // namespace Assemble

namespace Disassemble {

template <AddressingMode am>
concept has_imm = requires() {
    typename am::imm_type;
    requires std::integral<typename am::imm_type>;
};

auto disassemble(std::span<value_t> bytecode) {
    if (bytecode.size() == 0) {
        return ByteInstruction{nullptr};
    }

#define try_match(opcode, mne, given_am)                                                                               \
    case opcode:                                                                                                       \
        return ([&bytecode]<AddressingMode am>() -> ByteInstruction {                                                  \
                   if constexpr (is_one_of_v<am, AddressingType::Accumulator, AddressingType::Implied>) {              \
                       using Match = Assemble::to_impl<0, mne, am>::function;                                          \
                       if constexpr (Match::value) {                                                                   \
                           using Fn = Match::Fun;                                                                      \
                           return ByteInstruction{Fn{}}.add_mne(mne);                                                  \
                       }                                                                                               \
                   }                                                                                                   \
                   if constexpr (has_imm<am>) {                                                                        \
                       if constexpr (std::is_same_v<value_t, typename am::imm_type>) {                                 \
                           using Match = Assemble::to_impl<1, mne, am>::function;                                      \
                           if constexpr (Match::value) {                                                               \
                               value_t v = bytecode.size() > 1 ? bytecode[1] : value_t{0};                             \
                               using Fn = Match::Fun;                                                                  \
                               return ByteInstruction{std::bind(Fn{}, std::placeholders::_1, v)}.add_mne(mne);         \
                           }                                                                                           \
                       }                                                                                               \
                       if constexpr (std::is_same_v<address_t, typename am::imm_type>) {                               \
                           using Match = Assemble::to_impl<1, mne, am>::function;                                      \
                           if constexpr (Match::value) {                                                               \
                               address_t v = bytecode.size() > 2 ? ((bytecode[2] << 8) | bytecode[1]) : address_t{0};  \
                               using Fn = Match::Fun;                                                                  \
                               return ByteInstruction{std::bind(Fn{}, std::placeholders::_1, v)}.add_mne(mne);         \
                           }                                                                                           \
                       }                                                                                               \
                       if constexpr (std::is_same_v<svalue_t, typename am::imm_type>) {                                \
                           using Match = Assemble::to_impl<1, mne, am>::function;                                      \
                           if constexpr (Match::value) {                                                               \
                               svalue_t v = bytecode.size() > 1 ? bytecode[1] : svalue_t{0};                           \
                               using Fn = Match::Fun;                                                                  \
                               return ByteInstruction{std::bind(Fn{}, std::placeholders::_1, v)}.add_mne(mne);         \
                           }                                                                                           \
                       }                                                                                               \
                   }                                                                                                   \
                   return ByteInstruction{nullptr};                                                                    \
               })                                                                                                      \
            .template operator()<given_am>()

    switch (bytecode[0]) {
        try_match(0x00, "BRK", AddressingType::Implied);
        try_match(0x01, "ORA", AddressingType::IndirectX);
        try_match(0x05, "ORA", AddressingType::ZeroPage);
        try_match(0x06, "ASL", AddressingType::ZeroPage);
        try_match(0x08, "PHP", AddressingType::Implied);
        try_match(0x09, "ORA", AddressingType::Immediate);
        try_match(0x0A, "ASL", AddressingType::Accumulator);
        try_match(0x0D, "ORA", AddressingType::Absolute);
        try_match(0x0E, "ASL", AddressingType::Absolute);
        try_match(0x10, "BPL", AddressingType::Relative);
        try_match(0x11, "ORA", AddressingType::IndirectY);
        try_match(0x15, "ORA", AddressingType::ZeroPageX);
        try_match(0x16, "ASL", AddressingType::ZeroPageX);
        try_match(0x18, "CLC", AddressingType::Implied);
        try_match(0x19, "ORA", AddressingType::AbsoluteY);
        try_match(0x1D, "ORA", AddressingType::AbsoluteX);
        try_match(0x1E, "ASL", AddressingType::AbsoluteX);
        try_match(0x20, "JSR", AddressingType::Absolute);
        try_match(0x21, "AND", AddressingType::IndirectX);
        try_match(0x24, "BIT", AddressingType::ZeroPage);
        try_match(0x25, "AND", AddressingType::ZeroPage);
        try_match(0x26, "ROL", AddressingType::ZeroPage);
        try_match(0x28, "PLP", AddressingType::Implied);
        try_match(0x29, "AND", AddressingType::Immediate);
        try_match(0x2A, "ROL", AddressingType::Accumulator);
        try_match(0x2C, "BIT", AddressingType::Absolute);
        try_match(0x2D, "AND", AddressingType::Absolute);
        try_match(0x2E, "ROL", AddressingType::Absolute);
        try_match(0x30, "BMI", AddressingType::Relative);
        try_match(0x31, "AND", AddressingType::IndirectY);
        try_match(0x35, "AND", AddressingType::ZeroPageX);
        try_match(0x36, "ROL", AddressingType::ZeroPageX);
        try_match(0x38, "SEC", AddressingType::Implied);
        try_match(0x39, "AND", AddressingType::AbsoluteY);
        try_match(0x3D, "AND", AddressingType::AbsoluteX);
        try_match(0x3E, "ROL", AddressingType::AbsoluteX);
        try_match(0x40, "RTI", AddressingType::Implied);
        try_match(0x41, "EOR", AddressingType::IndirectX);
        try_match(0x45, "EOR", AddressingType::ZeroPage);
        try_match(0x46, "LSR", AddressingType::ZeroPage);
        try_match(0x48, "PHA", AddressingType::Implied);
        try_match(0x49, "EOR", AddressingType::Immediate);
        try_match(0x4A, "LSR", AddressingType::Accumulator);
        try_match(0x4C, "JMP", AddressingType::Absolute);
        try_match(0x4D, "EOR", AddressingType::Absolute);
        try_match(0x4E, "LSR", AddressingType::Absolute);
        try_match(0x50, "BVC", AddressingType::Relative);
        try_match(0x51, "EOR", AddressingType::IndirectY);
        try_match(0x55, "EOR", AddressingType::ZeroPageX);
        try_match(0x56, "LSR", AddressingType::ZeroPageX);
        try_match(0x58, "CLI", AddressingType::Implied);
        try_match(0x59, "EOR", AddressingType::AbsoluteY);
        try_match(0x5D, "EOR", AddressingType::AbsoluteX);
        try_match(0x5E, "LSR", AddressingType::AbsoluteX);
        try_match(0x60, "RTS", AddressingType::Implied);
        try_match(0x61, "ADC", AddressingType::IndirectX);
        try_match(0x65, "ADC", AddressingType::ZeroPage);
        try_match(0x66, "ROR", AddressingType::ZeroPage);
        try_match(0x68, "PLA", AddressingType::Implied);
        try_match(0x69, "ADC", AddressingType::Immediate);
        try_match(0x6A, "ROR", AddressingType::Accumulator);
        try_match(0x6C, "JMP", AddressingType::Absolute);
        try_match(0x6D, "ADC", AddressingType::Absolute);
        try_match(0x6E, "ROR", AddressingType::Absolute);
        try_match(0x70, "BVC", AddressingType::Relative);
        try_match(0x71, "ADC", AddressingType::IndirectY);
        try_match(0x75, "ADC", AddressingType::ZeroPageX);
        try_match(0x76, "ROR", AddressingType::ZeroPageX);
        try_match(0x78, "SEI", AddressingType::Implied);
        try_match(0x79, "ADC", AddressingType::AbsoluteY);
        try_match(0x7D, "ADC", AddressingType::AbsoluteX);
        try_match(0x7E, "ROR", AddressingType::AbsoluteX);
        try_match(0x81, "STA", AddressingType::IndirectX);
        try_match(0x84, "STY", AddressingType::ZeroPage);
        try_match(0x85, "STA", AddressingType::ZeroPage);
        try_match(0x86, "STX", AddressingType::ZeroPage);
        try_match(0x88, "DEY", AddressingType::Implied);
        try_match(0x8A, "TXA", AddressingType::Implied);
        try_match(0x8C, "STY", AddressingType::Absolute);
        try_match(0x8D, "STA", AddressingType::Absolute);
        try_match(0x8E, "STX", AddressingType::Absolute);
        try_match(0x90, "BCC", AddressingType::Relative);
        try_match(0x91, "STA", AddressingType::IndirectY);
        try_match(0x94, "STY", AddressingType::ZeroPageX);
        try_match(0x95, "STA", AddressingType::ZeroPageX);
        try_match(0x96, "STX", AddressingType::ZeroPageY);
        try_match(0x98, "TYA", AddressingType::Implied);
        try_match(0x99, "STA", AddressingType::AbsoluteY);
        try_match(0x9D, "STA", AddressingType::AbsoluteX);
        try_match(0xA0, "LDY", AddressingType::Immediate);
        try_match(0xA1, "LDA", AddressingType::IndirectX);
        try_match(0xA2, "LDX", AddressingType::Immediate);
        try_match(0xA4, "LDY", AddressingType::ZeroPage);
        try_match(0xA5, "LDA", AddressingType::ZeroPage);
        try_match(0xA6, "LDX", AddressingType::ZeroPage);
        try_match(0xA8, "TAY", AddressingType::Implied);
        try_match(0xA9, "LDA", AddressingType::Immediate);
        try_match(0xAA, "LDX", AddressingType::Accumulator);
        try_match(0xAC, "LDY", AddressingType::Absolute);
        try_match(0xAD, "LDA", AddressingType::Absolute);
        try_match(0xAE, "LDX", AddressingType::Absolute);
        try_match(0xB0, "BCS", AddressingType::Relative);
        try_match(0xB1, "LDA", AddressingType::IndirectY);
        try_match(0xB4, "LDY", AddressingType::ZeroPageX);
        try_match(0xB5, "LDA", AddressingType::ZeroPageX);
        try_match(0xB6, "LDX", AddressingType::ZeroPageY);
        try_match(0xB8, "CLV", AddressingType::Implied);
        try_match(0xB9, "LDA", AddressingType::AbsoluteY);
        try_match(0xBA, "TSX", AddressingType::Implied);
        try_match(0xBC, "LDY", AddressingType::AbsoluteX);
        try_match(0xBD, "LDA", AddressingType::AbsoluteX);
        try_match(0xBE, "LDX", AddressingType::AbsoluteY);
        try_match(0xC0, "CPY", AddressingType::Immediate);
        try_match(0xC1, "CMP", AddressingType::IndirectX);
        try_match(0xC4, "CPY", AddressingType::ZeroPage);
        try_match(0xC5, "CMP", AddressingType::ZeroPage);
        try_match(0xC6, "DEC", AddressingType::ZeroPage);
        try_match(0xC8, "INY", AddressingType::Implied);
        try_match(0xC9, "CMP", AddressingType::Immediate);
        try_match(0xCA, "DEX", AddressingType::Implied);
        try_match(0xCC, "CPY", AddressingType::Absolute);
        try_match(0xCD, "CMP", AddressingType::Absolute);
        try_match(0xCE, "DEC", AddressingType::Absolute);
        try_match(0xD0, "BNE", AddressingType::Relative);
        try_match(0xD1, "CMP", AddressingType::IndirectY);
        try_match(0xD5, "CMP", AddressingType::ZeroPageX);
        try_match(0xD6, "DEC", AddressingType::ZeroPageY);
        try_match(0xD8, "CLD", AddressingType::Implied);
        try_match(0xD9, "CMP", AddressingType::AbsoluteY);
        try_match(0xDD, "CMP", AddressingType::AbsoluteX);
        try_match(0xDE, "DEC", AddressingType::AbsoluteY);
        try_match(0xE0, "CPX", AddressingType::Immediate);
        try_match(0xE1, "SBC", AddressingType::IndirectX);
        try_match(0xE4, "CPX", AddressingType::ZeroPage);
        try_match(0xE5, "SBC", AddressingType::ZeroPage);
        try_match(0xE6, "INC", AddressingType::ZeroPage);
        try_match(0xE8, "INX", AddressingType::Implied);
        try_match(0xE9, "SBC", AddressingType::Immediate);
        try_match(0xEA, "NOP", AddressingType::Implied);
        try_match(0xEC, "CPX", AddressingType::Absolute);
        try_match(0xED, "SBC", AddressingType::Absolute);
        try_match(0xEE, "INC", AddressingType::Absolute);
        try_match(0xF0, "BNE", AddressingType::Relative);
        try_match(0xF1, "SBC", AddressingType::IndirectY);
        try_match(0xF5, "SBC", AddressingType::ZeroPageX);
        try_match(0xF6, "INC", AddressingType::ZeroPageY);
        try_match(0xF8, "SED", AddressingType::Implied);
        try_match(0xF9, "SBC", AddressingType::AbsoluteY);
        try_match(0xFD, "SBC", AddressingType::AbsoluteX);
        try_match(0xFE, "INC", AddressingType::AbsoluteY);
    }
#undef try_match
    return ByteInstruction{nullptr};
}

} // namespace Disassemble

} // namespace Instruction

auto step(Processor &proc) {
    auto ins = Instruction::Disassemble::disassemble(proc.memory.probe(proc.pc, 3));
    if (ins) {
        proc << ins;
    } else {
        std::cerr << "Error: Illegal instruction\n";
    }
    return ins;
}

} // namespace Fubuki

namespace std {
using namespace Fubuki::Instruction;
template <typename T>
    requires is_one_of_v<T, Param::A, Param::X, Param::Y, Param::Implied>
struct formatter<T> {
    constexpr auto parse(format_parse_context &ctx) {
        return ctx.begin();
    }
    auto format(T const &, format_context &ctx) const {
        return format_to(ctx.out(), "{} {{ }}", typeid(T).name());
    }
};

template <typename T>
    requires is_one_of_v<T, Param::Imm, Param::Address, Param::Label, Param::Offset>
struct formatter<T> {
    constexpr auto parse(format_parse_context &ctx) {
        return ctx.begin();
    }
    auto format(T const &t, format_context &ctx) const {
        return format_to(ctx.out(), "{} {{ {} }}", typeid(T).name(), t.store);
    }
};
} // namespace std

#include <cassert>
#include <iostream>

using namespace Fubuki::Instruction;
using namespace Fubuki::Instruction::AddressingType;

// NOLINTBEGIN

void test_instruction() {
    using Fubuki::Processor;
    {
        Processor proc;
        proc << i_(LoadA<Immediate, 0>{}, 0x64U);
        xxassert(proc.a == 0x64U);
    }
    {
        Processor proc;
        proc << i_(LoadX<Immediate, 0>{}, 0x64U);
        xxassert(proc.x == 0x64U);
    }
    {
        Processor proc;
        proc << i_(LoadY<Immediate, 0>{}, 0x64U);
        xxassert(proc.y == 0x64U);
    }
    {
        Processor proc;
        proc.memory.set_byte(3UL, 0x64U);
        proc << i_(LoadX<ZeroPage, 0>{}, 3UL);
        xxassert(proc.x == 0x64U);
    }
    {
        Processor proc;
        proc.memory.set_byte(3UL, 0x80U);
        proc << i_(LogicalBitTest<ZeroPage, 0>{}, 3U);
        xxassert(!proc.ps.flag.Z);
        xxassert(proc.ps.flag.N);
        xxassert(!proc.ps.flag.V);
    }
    {
        Processor proc;
        proc.memory.set_byte(3UL, 0x80U);
        proc << i_(LoadA<Immediate, 0>{}, 0xFFU) << i_(AddWithCarry<Immediate, 0>{}, 0x1U);
        xxassert(proc.ps.flag.Z);
        xxassert(!proc.ps.flag.N);
        xxassert(!proc.ps.flag.V);
        proc << i_(AddWithCarry<Immediate, 0>{}, 0x2U);
        xxassert(proc.a == 3U && !proc.ps.flag.C && !proc.ps.flag.V && !proc.ps.flag.Z && !proc.ps.flag.N);
        proc << i_(AddWithCarry<Immediate, 0>{}, 0x7FU);
        xxassert(proc.a == 0x82U && !proc.ps.flag.C && proc.ps.flag.V && !proc.ps.flag.Z && proc.ps.flag.N);
    }
    {
        Processor proc;
        proc << i_(Jump<Absolute, 0>{}, 0x200UL);
        xxassert(proc.pc == 0x200UL);
    }
    {
        Processor proc;
        proc << i_(LoadA<Immediate, 0>{}, 0xFEU) << i_(Jump<Absolute, 0>{}, 0x300UL)
             << i_(BranchMinus<Relative, 0>{}, 0x40U);
        xxassert(proc.pc == 0x342UL);
    }
}

void test_algorithm() {
    using Fubuki::Processor;
    {
        /**
         * Clearing memory
         */
        Processor proc;
        // printf("%0hhx %0hhx\n", *proc.memory.load_byte(0x10U), *proc.memory.load_byte(0x11U));
        proc.memory.set_byte(0x10U, 0xFE);
        proc.memory.set_byte(0x11U, 0xCA);
        proc << i_(LoadA<Immediate, 2>{}, 0U) << i_(StoreA<ZeroPage, 2>{}, 0x10U) << i_(StoreA<ZeroPage, 2>{}, 0x11U);
        xxassert(0 == *proc.memory.load_byte(0x10U) && 0 == *proc.memory.load_byte(0x11U));
    }
    {
        /**
         * Setting memory
         */
        Processor proc;
        proc << i_(LoadA<Immediate, 2>{}, 0xDEU) << i_(StoreA<ZeroPage, 2>{}, 0x10U)
             << i_(StoreA<ZeroPage, 2>{}, 0x11U);
        xxassert(0xDE == *proc.memory.load_byte(0x10U) && 0xDE == *proc.memory.load_byte(0x11U));
        // printf("%0hhx %0hhx\n", *proc.memory.load_byte(0x10U), *proc.memory.load_byte(0x11U));
    }
}

void test_assemble() {
    using namespace Fubuki::Instruction::Assemble;

    // TODO: Write my own std::variant
    // The current variant does not do overlap and thus the size is at least three times big!
    // std::cout << sizeof(Param::Params) << '\n';

    xxassert(std::holds_alternative<Param::A>(Param::parse("A")));
    xxassert(std::holds_alternative<Param::Imm>(Param::parse("#10")));
    // xxassert(std::get_if<Param::A>(Param::parse("#LO LABEL")));
    // xxassert(std::get_if<Param::A>(Param::parse("#HI LABEL")));
    {
        auto i1 = parse_instruction("LSR A");
        xxassert(str_equal<"LSR">(i1.mne));
        xxassert(std::holds_alternative<Param::A>(i1.parameter), i1.mne, i1.parameter);

        auto i2 = parse_instruction("LDA #10");
        // xxprint(i2.mne, std::get<Param::Imm>(i2.parameter));

        std::map<std::string, uint16_t> context;
        auto a1 = jit_listing(std::vector{i2, i1}, context, 0x200);
        Fubuki::Processor proc1;
        proc1 << a1[0];
        xxassert(proc1.a == 10, proc1.a, i2.parameter);
        proc1 << a1[1];
        xxassert(proc1.a == 5, proc1.a, i1.parameter);
        // xxassert(true, Param::Implied{});
    }

    {
        std::ifstream fd{"test.asm"};
        std::string str((std::istreambuf_iterator<char>(fd)), std::istreambuf_iterator<char>());
        auto [a2, ins, ctx] = parse_text(str);
        Fubuki::Processor proc2;
        proc2.memory.set_byte(0xFFFEU, 0xFE);
        proc2.memory.set_byte(0xFFFFU, 0xCA);
        xxassert(a2.size() == 8, a2.size());
        // for (auto i : ins) {
        //     xxprint(i.mne, i.parameter);
        // }
        for (auto i : a2) {
            proc2 << i;
        }
        xxassert(5 == proc2.a && 1 == proc2.x && 10 == proc2.y && 0xCAFE == proc2.pc, proc2.a, proc2.x, proc2.y,
                 proc2.pc);
    }

    {
        std::ifstream fd{"test2.asm"};
        std::string str2((std::istreambuf_iterator<char>(fd)), std::istreambuf_iterator<char>());
        auto [a3, ins3, ctx3] = parse_text(str2);
        Fubuki::Processor proc3;
        proc3.memory.set_byte(0xFFFEU, 0xFE);
        proc3.memory.set_byte(0xFFFFU, 0xCA);
        xxassert(ins3.size() == 7, ins3.size());
        // for (auto i : ins) {
        //     xxprint(i.mne, i.parameter);
        // }
        auto bin3 = assemble(ins3, ctx3, 0x200U);
        auto exp_bin3 =
            std::vector<Fubuki::value_t>{0xa9, 0x00, 0xa2, 0x0a, 0x86, 0x00, 0x65, 0x00, 0xca, 0xd0, 0xf9, 0x00};
        xxassert(bin3 == exp_bin3);
        load_to_memory(proc3, 0x200U, bin3);
        proc3.pc = 0x200U;
        while (1) {
            auto last_instruction = Fubuki::step(proc3);
            if (str_equal<"BRK">(last_instruction.metadata_mne))
                break;
        }

        xxassert(55 == proc3.a && 0 == proc3.x && 0xCAFE == proc3.pc, proc3.a, proc3.x, proc3.pc);
    }
}

// NOLINTEND

int main() {
    test_instruction();
    test_algorithm();
    test_assemble();
    return 0;
}