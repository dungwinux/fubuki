#pragma once

#include <algorithm>
#include <charconv>
#include <format>
#include <iostream>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "xxassert.hpp"

#if defined(__clang__) || defined(__GNUC__)
#define unreachable() __builtin_unreachable()
#define ror8(x, y) __builtin_rotateleft8(x, y)
#define rol8(x, y) __builtin_rotateright8(x, y)
#elif defined(_MSC_VER)
#include <intrin.h>
#define unreachable() __assume(0)
#pragma intrinsic(_rotr8, _rotl8)
#define ror8(x, y) _rotr8(x, y)
#define rol8(x, y) _rotl8(x, y)
#else
#endif

template <typename...>
struct is_one_of : std::false_type {};
template <typename T>
struct is_one_of<T> : std::false_type {};
template <typename T, typename... List>
struct is_one_of<T, List...> : std::disjunction<std::is_same<T, List>...> {};

template <typename... List>
constexpr bool is_one_of_v = is_one_of<List...>::value;

/**
 * Based on: https://ctrpeach.io/posts/cpp20-string-literal-template-parameters/
 */
template <size_t N>
struct TpString {
    constexpr static const size_t size = N;
    // Be careful, this also copy NULL terminating bytes
    constexpr TpString(const char (&str)[N]) {
        std::copy_n(str, N, value);
    }
    char value[N];
    // size_t length{N};
};

template <TpString lhs, TpString rhs>
struct tpstr_equal {
    constexpr static const bool value = decltype(lhs)::size == decltype(rhs)::size && std::string_view{lhs.value} == rhs.value;
};
template <TpString T, TpString U>
constexpr bool tpstr_equal_v = tpstr_equal<T, U>::value;

template <TpString known>
constexpr inline auto str_equal(std::string_view comparison) -> bool {
    return 0 == _stricmp(known.value, comparison.data());
};

template <TpString... args>
constexpr inline auto str_inlist(std::string_view comparison) -> bool {
    return (str_equal<args>(comparison) || ...);
}

template <>
constexpr inline auto str_inlist(std::string_view) -> bool {
    return false;
}

template <typename R>
    requires requires(R r) {
        { *r.data() } -> std::convertible_to<char>;
    }
constexpr inline std::string_view to_sv(R &&r) {
    return {r.data(), r.size()};
}
auto cast_sv = std::views::transform([](auto x) -> auto { return to_sv(x); });
template <typename R>
constexpr inline std::vector<typename std::ranges::range_value_t<R>> to_vec(R &&r) {
    return {std::begin(r), std::end(r)};
}

template <typename T, int base = 16>
inline std::optional<T> parse_int(std::string_view s) {
    T value{};
    if (std::from_chars(s.data(), s.data() + s.size(), value, base).ec == std::errc{}) {
        return value;
    }
    return {};
}

constexpr auto const not_empty = [](auto s) -> bool {
    // std::cout << '\n' << typeid(s).name() << '\n';
    return !s.empty();
};
constexpr auto const trim_left = [](std::string_view s) -> auto {
    size_t index;
    for (index = 0; index < s.length() && s[index] == ' '; ++index) {
    }
    return index == s.length() ? s : s.substr(index);
};
constexpr auto const trim_right = [](std::string_view s) -> auto {
    size_t index;
    for (index = s.length(); index-- > 0 && s[index] == ' ';) {
    }
    return index == 0 ? s : s.substr(0, index + 1);
};
constexpr auto const trim = [](std::string_view s) -> auto { return trim_left(trim_right(s)); };

template <typename R>
constexpr inline std::string str_concat(R &&r) {
    std::string out;
    out.reserve(128);
    for (auto e : r) {
        if (e.size() != 0) {
            out += to_sv(e);
        }
    }
    return out;
}
// struct Error {
//     char const *msg;
//     Error(decltype(msg) &&s) : msg(s) {};
// };

template <std::integral T>
inline auto is_representable(std::integral auto const &x) {
    using X = decltype(x);
    return static_cast<X>(static_cast<T>(x)) == x;
}

// hacky way
// template <typename T>
// concept is_formmatable = requires(T &&t) { std::format("{}", t); };

template <typename... Args>
struct std::formatter<std::variant<Args...>> {
    constexpr auto parse(std::format_parse_context &ctx) {
        return ctx.begin();
    }
    auto format(std::variant<Args...> const &obj, std::format_context &ctx) const {
        auto get = std::visit(
            []<typename T>(T &&arg) -> auto {
                if constexpr (std::formattable<T, std::format_context::char_type>) {
                    return std::format("<{}> {}", typeid(arg).name(), arg);
                } else {
                    return std::format("<{}>", typeid(arg).name());
                }
            },
            obj);
        return std::format_to(ctx.out(), "{}", get);
    }
};

template <class... Ts>
struct overloads : Ts... {
    using Ts::operator()...;
};

// template <typename... Extends, typename F, typename... Args>
//     requires std::invocable<F, Args...>
// inline auto inverse_bind_front(F &&fn) {
//     return [&](Args... args, Extends...) {
//         return std::invoke(fn, args...);
//     };
// }

// template <typename T, typename... Extends>
// struct inverse_bind_front_ft: public T {

//     template <typename ...Args>
//     requires std::invocable<T::operator(), T, Args...>
//     overload auto operator()(Args&&...args, Extends...) {
//         std::invoke(T::operator(), this, args...);
//     }
// };