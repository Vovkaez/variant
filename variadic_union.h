#pragma once
#include <cstddef>
#include <utility>

namespace details {

template <typename...>
struct variadic_union;

template <typename T>
struct is_variadic_union_helper : std::false_type {};

template <typename T>
struct is_variadic_union_helper<const T> : is_variadic_union_helper<T> {};

template <typename... Types>
struct is_variadic_union_helper<variadic_union<Types...>> : std::true_type {};

template <typename T>
concept is_variadic_union = is_variadic_union_helper<T>::value;

struct construct_empty_t {};
inline constexpr construct_empty_t construct_empty;

template <typename Head>
struct variadic_union<Head> {
  constexpr variadic_union() : value{} {};
  explicit constexpr variadic_union(construct_empty_t) {}
  constexpr ~variadic_union() {}

  union {
    Head value;
  };
};

template <typename Head>
requires(std::is_trivially_destructible_v<Head>) struct variadic_union<Head> {
  constexpr variadic_union() : value{} {};
  explicit constexpr variadic_union(construct_empty_t) {}

  union {
    Head value;
  };
};

template <typename Head, typename... Tail>
struct variadic_union<Head, Tail...> {
  constexpr variadic_union() : value{} {}
  explicit constexpr variadic_union(construct_empty_t) : tail(construct_empty) {}
  constexpr ~variadic_union() {}

  union {
    Head value;
    variadic_union<Tail...> tail;
  };
};

template <typename Head, typename... Tail>
requires(std::is_trivially_destructible_v<Head> &&
         (std::is_trivially_destructible_v<Tail> && ...)) struct variadic_union<Head, Tail...> {
  constexpr variadic_union() : value{} {}
  explicit constexpr variadic_union(construct_empty_t) : tail(construct_empty) {}

  union {
    Head value;
    variadic_union<Tail...> tail;
  };
};

template <size_t I, typename Union>
requires(is_variadic_union<std::remove_cvref_t<Union>>) constexpr decltype(auto) get(Union&& un) noexcept {
  if constexpr (I == 0) {
    return (std::forward<Union>(un).value);
  } else {
    return get<I - 1>(std::forward<Union>(un).tail);
  }
}

} // namespace details
