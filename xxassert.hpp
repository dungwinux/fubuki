#pragma once

#include <cassert>
#include <format>
#include <iostream>
#include <string_view>

template <typename T>
inline void debugprint(T &&t, std::string_view s) {
    std::cout << std::format("[{}] {} = {}\n", typeid(t).name(), s, t);
}

// Based on https://www.scs.stanford.edu/~dm/blog/va-opt.html
#define PAREN ()
#define EXPAND(arg) EXPAND1(EXPAND1(EXPAND1(EXPAND1(arg))))
#define EXPAND1(arg) EXPAND2(EXPAND2(EXPAND2(EXPAND2(arg))))
#define EXPAND2(arg) EXPAND3(EXPAND3(EXPAND3(EXPAND3(arg))))
#define EXPAND3(arg) EXPAND4(EXPAND4(EXPAND4(EXPAND4(arg))))
#define EXPAND4(arg) arg

#ifdef NDEBUG
#define xxassert(...) (void)0
#define xxprint(...) (void)0
#else
#define xxassert(cond, ...)                                                                                            \
    if (!(cond)) {                                                                                                     \
        xxprint(__VA_ARGS__);                                                                                          \
    }                                                                                                                  \
    assert(cond)
#define xxprint(...) __VA_OPT__(EXPAND(xxprint_helper(__VA_ARGS__)))
#define xxprint_helper(v, ...) xprint(v) __VA_OPT__(; xxprint_again PAREN(__VA_ARGS__))
#define xxprint_again() xxprint_helper
#define xprint(v) debugprint(v, #v)
#endif
