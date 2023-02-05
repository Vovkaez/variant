#pragma once
#include "variant_helpers.h"

namespace details {

struct pass_indexes_to_visitor_t {};
inline constexpr pass_indexes_to_visitor_t pass_indexes_to_visitor{};

template <size_t Depth, typename R, typename Visitor, typename... Variants>
struct multi_table;

template <size_t Depth, typename R, typename Visitor, typename V1, typename... Rest>
requires(Depth < sizeof...(Rest) + 1) struct multi_table<Depth, R, Visitor, V1, Rest...> {
private:
  using inner_t = multi_table<Depth + 1, R, Visitor, V1, Rest...>;
  static constexpr size_t size = variant_size_v<std::remove_cvref_t<V1>>;

public:
  constexpr multi_table() : multi_table(std::index_sequence<>{}) {}
  explicit constexpr multi_table(pass_indexes_to_visitor_t)
      : multi_table(pass_indexes_to_visitor, std::index_sequence<>{}) {}

  template <size_t... Indexes>
  constexpr explicit multi_table(std::index_sequence<Indexes...>) noexcept
      : arr([]<size_t... LastIndexValue>(std::index_sequence<LastIndexValue...>) {
          return std::array{inner_t(append<LastIndexValue>(std::index_sequence<Indexes...>{}))...};
        }(std::make_index_sequence<size>{})) {}

  template <size_t... Indexes>
  constexpr explicit multi_table(pass_indexes_to_visitor_t, std::index_sequence<Indexes...>) noexcept
      : arr([]<size_t... LastIndexValue>(std::index_sequence<LastIndexValue...>) {
          return std::array{
              inner_t(pass_indexes_to_visitor, append<LastIndexValue>(std::index_sequence<Indexes...>{}))...};
        }(std::make_index_sequence<size>{})) {}

  template <typename Head, typename... Tail>
  constexpr auto at(Head&& i, Tail&&... tail) const {
    return arr[i].at(std::forward<Tail>(tail)...);
  }

private:
  std::array<inner_t, size> arr;
};

template <size_t Depth, typename R, typename Visitor, typename... Variants>
requires(Depth == sizeof...(Variants)) struct multi_table<Depth, R, Visitor, Variants...> {
private:
  using func_ptr_t = R (*)(Visitor&&, Variants&&...);

public:
  template <size_t... Indexes>
  constexpr explicit multi_table(std::index_sequence<Indexes...>) noexcept
      : f([](Visitor&& visitor, Variants&&... variants) {
          return std::forward<Visitor>(visitor)(get<Indexes>(std::forward<Variants>(variants))...);
        }) {}

  template <size_t... Indexes>
  constexpr explicit multi_table(pass_indexes_to_visitor_t, std::index_sequence<Indexes...>) noexcept
      : f([](Visitor&& visitor, Variants&&... variants) {
          return std::forward<Visitor>(visitor)(std::index_sequence<Indexes...>{},
                                                get<Indexes>(std::forward<Variants>(variants))...);
        }) {}

  constexpr func_ptr_t at() const noexcept {
    return f;
  }

private:
  func_ptr_t f;
};

template <size_t Depth, typename R, typename Visitor, typename... Variants>
inline constexpr multi_table<Depth, R, Visitor, Variants...> visit_table;

template <size_t Depth, typename R, typename Visitor, typename... Variants>
inline constexpr multi_table<Depth, R, Visitor, Variants...> visit_with_indexes_table(pass_indexes_to_visitor);

template <typename Visitor, typename... Variants>
requires(details::are_variants_remove_ref<Variants...>) constexpr details::visit_with_indexes_return_type_t<
    Visitor, Variants...> visit_with_indexes(Visitor&& visitor, Variants&&... variants) {
  using R = details::visit_with_indexes_return_type_t<Visitor, Variants...>;
  return details::visit_with_indexes_table<0, R, Visitor, Variants...>.at(variants.index()...)(
      std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}
} // namespace details

template <typename Visitor, typename... Variants>
constexpr details::visit_return_type_t<Visitor, Variants...> visit(Visitor&& visitor, Variants&&... variants) {
  if ((std::forward<Variants>(variants).valueless_by_exception() || ...)) {
    throw bad_variant_access("visit: variant is valueless");
  }
  using R = details::visit_return_type_t<Visitor, Variants...>;
  return details::visit_table<0, R, Visitor, Variants...>.at(variants.index()...)(std::forward<Visitor>(visitor),
                                                                                  std::forward<Variants>(variants)...);
}
