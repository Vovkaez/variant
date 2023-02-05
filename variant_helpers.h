#pragma once

#include <array>
#include <concepts>
#include <cstddef>
#include <utility>

inline constexpr size_t variant_npos = -1;

struct bad_variant_access : std::exception {
  explicit bad_variant_access(char const* text) noexcept : text(text) {}

  char const* what() const noexcept override {
    return text;
  }

private:
  char const* text;
};

namespace details {
template <typename...>
struct variant_base;
}

template <typename... Types>
struct variant;

template <typename T>
struct in_place_type_t {};

template <typename T>
inline constexpr in_place_type_t<T> in_place_type{};

template <size_t I>
struct in_place_index_t {};

template <size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

template <typename T>
struct variant_size;

template <typename T>
struct variant_size<const T> : variant_size<T> {};

template <typename... Types>
struct variant_size<details::variant_base<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};

template <typename T>
constexpr size_t variant_size_v = variant_size<T>::value;

namespace details {
template <typename T>
struct is_variant_helper : std::false_type {};

template <typename T>
struct is_variant_helper<const T> : is_variant_helper<T> {};

template <typename... Types>
struct is_variant_helper<variant<Types...>> : std::true_type {};

template <typename T>
concept is_variant = is_variant_helper<T>::value;

template <typename... Types>
concept are_variants_remove_ref = (is_variant<std::remove_reference_t<Types>> && ...);

template <size_t I, typename... Types>
struct nth_type;

template <typename Head, typename... Tail>
struct nth_type<0, Head, Tail...> {
  using type = Head;
};

template <size_t I, typename Head, typename... Tail>
struct nth_type<I, Head, Tail...> {
  using type = typename nth_type<I - 1, Tail...>::type;
};

template <size_t I, typename... Types>
using nth_type_t = typename nth_type<I, Types...>::type;

template <typename T, typename... Types>
struct count_appearances;

template <typename T, typename... Types>
constexpr size_t count_appearances_v = count_appearances<T, Types...>::value;

template <typename T>
struct count_appearances<T> : std::integral_constant<size_t, 0> {};

template <typename T, typename Head, typename... Tail>
struct count_appearances<T, Head, Tail...>
    : std::integral_constant<size_t, std::is_same_v<T, Head> + count_appearances_v<T, Tail...>> {};

template <typename T, typename... Types>
concept appears_once = count_appearances_v<T, Types...> == 1;

template <typename T, typename... Types>
requires appears_once<T, Types...> struct find_type;

template <typename T, typename... Types>
constexpr size_t find_type_v = find_type<T, Types...>::value;

template <typename T, std::same_as<T> Head, typename... Tail>
struct find_type<T, Head, Tail...> : std::integral_constant<size_t, 0> {};

template <typename T, typename Head, typename... Tail>
struct find_type<T, Head, Tail...> : std::integral_constant<size_t, find_type_v<T, Tail...> + 1> {};

template <size_t Last, size_t... Current>
constexpr std::index_sequence<Current..., Last> append(std::index_sequence<Current...>) {
  return {};
}
} // namespace details

template <size_t I, typename T>
struct variant_alternative;

template <size_t I, typename T>
using variant_alternative_t = typename variant_alternative<I, T>::type;

template <size_t I, typename T>
struct variant_alternative<I, const T> {
  using type = std::add_const_t<variant_alternative_t<I, T>>;
};

template <size_t I, typename... Types>
struct variant_alternative<I, details::variant_base<Types...>> {
  using type = details::nth_type_t<I, Types...>;
};

template <size_t I, typename... Types>
struct variant_alternative<I, variant<Types...>> {
  using type = details::nth_type_t<I, Types...>;
};

namespace details {
template <size_t I, typename Variant, typename Alt = variant_alternative_t<I, std::remove_reference_t<Variant>>>
using get_t = std::conditional_t<std::is_reference_v<Variant>, Alt&, Alt&&>;

template <typename Visitor, typename... Variants>
using visit_return_type_t = std::invoke_result_t<Visitor, get_t<0, Variants>...>;

template <typename Visitor, typename... Variants>
using visit_with_indexes_return_type_t =
    std::invoke_result_t<Visitor, std::make_index_sequence<sizeof...(Variants)>, get_t<0, Variants>...>;

template <typename T, typename T_i>
concept convert_constraint = requires(T && y) {
  std::array<T_i, 1>{std::forward<T>(y)};
};

template <typename T, typename... Types>
struct func_with_overloads {
  constexpr void operator()() {}
};

template <typename T, typename Head, typename... Tail>
struct func_with_overloads<T, Head, Tail...> : func_with_overloads<T, Tail...> {
  using func_with_overloads<T, Tail...>::operator();
};

template <typename T, typename Head, typename... Tail>
requires convert_constraint<T, Head> struct func_with_overloads<T, Head, Tail...> : func_with_overloads<T, Tail...> {
  constexpr Head operator()(Head) {}
  using func_with_overloads<T, Tail...>::operator();
};

template <typename T, typename... Types>
struct selected_overload {
  using type = void;
};

template <typename T, typename... Types>
requires std::invocable<func_with_overloads<T, Types...>, T> struct selected_overload<T, Types...> {
  using type = decltype(std::declval<func_with_overloads<T, Types...>>()(std::forward<T>(std::declval<T>())));
};

template <typename T, typename... Types>
using selected_overload_t = typename selected_overload<T, Types...>::type;
} // namespace details
