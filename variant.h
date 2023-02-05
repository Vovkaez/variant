#pragma once
#include "variadic_union.h"
#include "variant_helpers.h"
#include "visit.h"
#include <concepts>
#include <cstddef>
#include <memory>
#include <tuple>
#include <utility>

namespace details {
template <typename... Types>
struct variant_base {
  constexpr variant_base() : data(), index_(0) {}
  constexpr explicit variant_base(size_t index) : data(details::construct_empty), index_(index) {}

  constexpr ~variant_base() {
    if (index_ != variant_npos) {
      visit([]<typename T>(T& value) { value.~T(); }, *this);
    }
  }

  constexpr bool valueless_by_exception() const noexcept {
    return this->index_ == variant_npos;
  }

  constexpr size_t index() const noexcept {
    return this->index_;
  }

  template <size_t I>
  requires(I < sizeof...(Types)) friend constexpr variant_alternative_t<I, variant_base>& get(variant_base& var) {
    return get<I>(var.data);
  }

protected:
  details::variadic_union<Types...> data;
  size_t index_;
};

template <typename... Types>
requires(std::is_trivially_destructible_v<Types>&&...) struct variant_base<Types...> {
  constexpr variant_base() : data(), index_(0) {}
  constexpr explicit variant_base(size_t index) : data(details::construct_empty), index_(index) {}

  constexpr bool valueless_by_exception() const noexcept {
    return this->index_ == variant_npos;
  }

  constexpr size_t index() const noexcept {
    return this->index_;
  }

protected:
  details::variadic_union<Types...> data;
  size_t index_;
};
} // namespace details

template <typename... Types>
struct variant : details::variant_base<Types...> {
private:
  using base = details::variant_base<Types...>;
  using T_0 = details::nth_type_t<0, Types...>;
  static constexpr bool alts_are_trivially_destructible = (std::is_trivially_destructible_v<Types> && ...);
  static constexpr bool alts_are_trivially_copy_constructible = (std::is_trivially_copy_constructible_v<Types> && ...);
  static constexpr bool alts_are_copy_constructible = (std::is_copy_constructible_v<Types> && ...);
  static constexpr bool alts_are_trivially_move_constructible = (std::is_trivially_move_constructible_v<Types> && ...);
  static constexpr bool alts_are_move_constructible = (std::is_move_constructible_v<Types> && ...);
  static constexpr bool alts_are_trivially_copy_assignable = (std::is_trivially_copy_assignable_v<Types> && ...);
  static constexpr bool alts_are_copy_assignable = (std::is_copy_assignable_v<Types> && ...);
  static constexpr bool alts_are_trivially_move_assignable = (std::is_trivially_move_assignable_v<Types> && ...);
  static constexpr bool alts_are_move_assignable = (std::is_move_assignable_v<Types> && ...);

  static constexpr bool variant_is_trivially_copy_assignable =
      alts_are_trivially_copy_constructible && alts_are_trivially_copy_assignable && alts_are_trivially_destructible;
  static constexpr bool variant_is_trivially_move_assignable =
      alts_are_trivially_move_constructible && alts_are_trivially_move_assignable && alts_are_trivially_destructible;

public:
  constexpr variant() noexcept(std::is_nothrow_constructible_v<T_0>)
      requires(std::is_default_constructible_v<T_0>) = default;

  constexpr variant(variant const& other) noexcept requires(alts_are_trivially_copy_constructible) = default;
  constexpr variant(variant const& other) noexcept((std::is_nothrow_copy_constructible_v<Types> && ...))
      requires(!alts_are_trivially_copy_constructible && alts_are_copy_constructible)
      : base(other.index_) {
    if (!this->valueless_by_exception()) {
      details::visit_with_indexes(
          [&]<size_t I>(std::index_sequence<I>, auto& this_value) {
            std::construct_at(std::addressof(this_value), details::get<I>(other.data));
          },
          *this);
    }
  }

  constexpr variant(variant&& other) noexcept requires(alts_are_trivially_move_constructible) = default;
  constexpr variant(variant&& other) noexcept((std::is_nothrow_move_constructible_v<Types> && ...))
      requires(!alts_are_trivially_move_constructible && alts_are_move_constructible)
      : base(other.index_) {
    if (!this->valueless_by_exception()) {
      details::visit_with_indexes(
          [&]<size_t I>(std::index_sequence<I>, auto& this_value) {
            std::construct_at(std::addressof(this_value), details::get<I>(std::move(other).data));
          },
          *this);
    }
  }

  template <typename T>
  requires(sizeof...(Types) > 0 && !std::is_same_v<std::remove_cvref_t<T>, variant> &&
           std::is_constructible_v<
               details::selected_overload_t<T, Types...>,
               T>) constexpr variant(T&& t) noexcept(std::
                                                         is_nothrow_constructible_v<
                                                             details::selected_overload_t<T, Types...>, T>)
      : variant(in_place_type<details::selected_overload_t<T, Types...>>, std::forward<T>(t)) {}

  template <typename T, typename... Args>
  explicit constexpr variant(in_place_type_t<T>, Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args...>)
      requires(details::appears_once<T, Types...>&& std::is_constructible_v<T, Args...>)
      : base(variant_npos) {
    emplace<T>(std::forward<Args>(args)...);
  }

  template <typename T, typename U, typename... Args>
  explicit constexpr variant(in_place_type_t<T>, std::initializer_list<U> il,
                             Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args...>)
      requires(details::appears_once<T, Types...>&& std::is_constructible_v<T, std::initializer_list<U>&, Args...>)
      : base(variant_npos) {
    emplace<T>(il, std::forward<Args>(args)...);
  }

  template <size_t I, typename... Args>
  requires(I < sizeof...(Types) &&
           std::is_constructible_v<
               details::nth_type_t<I, Types...>,
               Args...>) explicit constexpr variant(in_place_index_t<I>,
                                                    Args&&... args) noexcept(std::
                                                                                 is_nothrow_constructible_v<
                                                                                     details::nth_type_t<I, Types...>,
                                                                                     Args...>)
      : base(variant_npos) {
    emplace<I>(std::forward<Args>(args)...);
  }

  template <size_t I, typename U, typename... Args>
  requires(I < sizeof...(Types) &&
           std::is_constructible_v<
               details::nth_type_t<I, Types...>, std::initializer_list<U>&,
               Args...>) explicit constexpr variant(in_place_index_t<I>, std::initializer_list<U> il,
                                                    Args&&... args) noexcept(std::
                                                                                 is_nothrow_constructible_v<
                                                                                     details::nth_type_t<I, Types...>,
                                                                                     Args...>)
      : base(variant_npos) {
    emplace<I>(il, std::forward<Args>(args)...);
  }

  constexpr variant& operator=(variant const& other) noexcept requires(variant_is_trivially_copy_assignable) = default;
  constexpr variant& operator=(variant const& other)
      requires(!variant_is_trivially_copy_assignable && alts_are_copy_constructible && alts_are_copy_assignable) {
    if (other.valueless_by_exception()) {
      destroy();
    } else if (this->index_ == other.index_) {
      details::visit_with_indexes(
          [&]<size_t I>(std::index_sequence<I>, auto const& other_value) { details::get<I>(this->data) = other_value; },
          other);
    } else {
      details::visit_with_indexes(
          [&]<size_t I, typename T>(std::index_sequence<I>, T const& other_value) {
            if constexpr (std::is_nothrow_copy_constructible_v<T> || !std::is_nothrow_move_constructible_v<T>) {
              emplace<I>(other_value);
            } else {
              emplace<I>(T(other_value));
            }
          },
          other);
    }
    return *this;
  }

  consteval variant& operator=(variant&& other) noexcept requires(variant_is_trivially_move_assignable) = default;
  constexpr variant& operator=(variant&& other) noexcept(((std::is_nothrow_move_constructible_v<Types> &&
                                                           std::is_nothrow_move_assignable_v<Types>)&&...))
      requires(!variant_is_trivially_move_assignable && alts_are_move_constructible && alts_are_move_assignable) {
    if (other.valueless_by_exception()) {
      destroy();
    } else if (this->index_ == other.index_) {
      details::visit_with_indexes(
          [&]<size_t I>(std::index_sequence<I>, auto& other_value) {
            details::get<I>(this->data) = std::move(other_value);
          },
          other);
    } else {
      details::visit_with_indexes(
          [&]<size_t I>(std::index_sequence<I>, auto& other_value) { emplace<I>(std::move(other_value)); }, other);
    }
    return *this;
  }

  template <typename T>
  requires(!std::is_same_v<std::remove_cvref_t<T>, variant> &&
           std::is_assignable_v<details::selected_overload_t<T, Types...>&, T> &&
           std::is_constructible_v<details::selected_overload_t<T, Types...>, T>) constexpr variant&
  operator=(T&& t) noexcept(std::is_nothrow_assignable_v<details::selected_overload_t<T, Types...>&, T>&&
                                std::is_nothrow_constructible_v<details::selected_overload_t<T, Types...>, T>) {
    using T_i = details::selected_overload_t<T, Types...>;
    constexpr size_t I = details::find_type_v<T_i, Types...>;
    if (this->index() == I) {
      get<I>(*this) = std::forward<T>(t);
      return *this;
    }
    if constexpr (std::is_nothrow_constructible_v<details::selected_overload_t<T, Types...>&, T> ||
                  !std::is_nothrow_move_constructible_v<details::selected_overload_t<T, Types...>>) {
      emplace<I>(std::forward<T>(t));
    } else {
      emplace<I>(T_i(std::forward<T>(t)));
    }
    return *this;
  }

  template <typename T, typename... Args>
  requires(details::appears_once<T, Types...>&& std::is_constructible_v<T, Args...>) constexpr T& emplace(
      Args&&... args) {
    return emplace<details::find_type_v<T, Types...>>(std::forward<Args>(args)...);
  }

  template <typename T, typename U, typename... Args>
  requires(details::appears_once<T, Types...>&& std::is_constructible_v<
           T, std::initializer_list<U>&, Args...>) constexpr T& emplace(std::initializer_list<U> il, Args&&... args) {
    return emplace<details::find_type_v<T, Types...>>(std::move(il), std::forward<Args>(args)...);
  }

  template <size_t I, typename... Args>
  requires(I < sizeof...(Types) &&
           std::is_constructible_v<details::nth_type_t<I, Types...>,
                                   Args...>) constexpr variant_alternative_t<I, variant>& emplace(Args&&... args) {
    destroy();
    std::construct_at(std::addressof(details::get<I>(this->data)), std::forward<Args>(args)...);
    this->index_ = I;
    return details::get<I>(this->data);
  }

  template <size_t I, typename U, typename... Args>
  requires(I < sizeof...(Types) &&
           std::is_constructible_v<
               details::nth_type_t<I, Types...>, std::initializer_list<U>&,
               Args...>) constexpr variant_alternative_t<I, variant>& emplace(std::initializer_list<U> il,
                                                                              Args&&... args) {
    destroy();
    std::construct_at(std::addressof(details::get<I>(this->data)), il, std::forward<Args>(args)...);
    this->index_ = I;
    return details::get<I>(this->data);
  }

  constexpr void swap(variant& other) noexcept(((std::is_nothrow_move_constructible_v<Types> &&
                                                 std::is_nothrow_swappable_v<Types>)&&...))
      requires(alts_are_move_constructible) {
    if (this->valueless_by_exception()) {
      if (!other.valueless_by_exception()) {
        *this = std::move(other);
        other.destroy();
      }
      return;
    }
    if (this->index_ == other.index_) {
      details::visit_with_indexes(
          [&]<size_t I>(std::index_sequence<I>, auto& other_value) {
            using std::swap;
            swap(details::get<I>(this->data), other_value);
          },
          other);
    } else {
      std::swap(*this, other);
    }
  }

  friend constexpr bool operator==(variant const& v1, variant const& v2) {
    if (v1.index_ != v2.index_) {
      return false;
    }
    if (v1.valueless_by_exception()) {
      return true;
    }
    return details::visit_with_indexes(
        [&]<size_t I>(std::index_sequence<I>, auto const& v1_value) { return v1_value == get<I>(v2.data); }, v1);
  }

  friend constexpr bool operator!=(variant const& v1, variant const& v2) {
    if (v1.index_ != v2.index_) {
      return true;
    }
    if (v1.valueless_by_exception()) {
      return false;
    }
    return details::visit_with_indexes(
        [&]<size_t I>(std::index_sequence<I>, auto const& v1_value) { return v1_value != get<I>(v2.data); }, v1);
  }

  friend constexpr bool operator<(variant const& v1, variant const& v2) {
    if (v2.valueless_by_exception()) {
      return false;
    }
    if (v1.valueless_by_exception()) {
      return true;
    }
    if (v1.index_ < v2.index_) {
      return true;
    }
    if (v1.index_ > v2.index_) {
      return false;
    }
    return details::visit_with_indexes(
        [&]<size_t I>(std::index_sequence<I>, auto const& v1_value) { return v1_value < get<I>(v2.data); }, v1);
  }

  friend constexpr bool operator>(variant const& v1, variant const& v2) {
    if (v1.valueless_by_exception()) {
      return false;
    }
    if (v2.valueless_by_exception()) {
      return true;
    }
    if (v1.index_ > v2.index_) {
      return true;
    }
    if (v1.index_ < v2.index_) {
      return false;
    }
    return details::visit_with_indexes(
        [&]<size_t I>(std::index_sequence<I>, auto const& v1_value) { return v1_value > get<I>(v2.data); }, v1);
  }

  friend constexpr bool operator<=(variant const& v1, variant const& v2) {
    if (v1.valueless_by_exception()) {
      return true;
    }
    if (v2.valueless_by_exception()) {
      return false;
    }
    if (v1.index_ < v2.index_) {
      return true;
    }
    if (v1.index_ > v2.index_) {
      return false;
    }
    return details::visit_with_indexes(
        [&]<size_t I>(std::index_sequence<I>, auto const& v1_value) { return v1_value <= get<I>(v2.data); }, v1);
  }

  friend constexpr bool operator>=(variant const& v1, variant const& v2) {
    if (v2.valueless_by_exception()) {
      return true;
    }
    if (v1.valueless_by_exception()) {
      return false;
    }
    if (v1.index_ > v2.index_) {
      return true;
    }
    if (v1.index_ < v2.index_) {
      return false;
    }
    return details::visit_with_indexes(
        [&]<size_t I>(std::index_sequence<I>, auto const& v1_value) { return v1_value >= get<I>(v2.data); }, v1);
  }

private:
  constexpr void destroy() noexcept {
    if (!this->valueless_by_exception()) {
      visit([]<typename T>(T& value) { value.~T(); }, *this);
      this->index_ = variant_npos;
    }
  }

  template <size_t I>
  constexpr void ensure_valid_get() const {
    if (this->valueless_by_exception()) {
      throw bad_variant_access("get: variant is valueless");
    } else if (this->index() != I) {
      throw bad_variant_access("get: wrong index for variant");
    }
  }

  template <size_t I>
  requires(I < sizeof...(Types)) friend constexpr variant_alternative_t<I, variant>& get(variant& var) {
    var.ensure_valid_get<I>();
    return get<I>(var.data);
  }

  template <size_t I>
  requires(I < sizeof...(Types)) friend constexpr variant_alternative_t<I, variant>&& get(variant&& var) {
    var.ensure_valid_get<I>();
    return get<I>(std::move(var.data));
  }

  template <size_t I>
  requires(I < sizeof...(Types)) friend constexpr variant_alternative_t<I, variant> const& get(const variant& var) {
    var.ensure_valid_get<I>();
    return get<I>(var.data);
  }

  template <size_t I>
  requires(I < sizeof...(Types)) friend constexpr variant_alternative_t<I, variant> const&& get(const variant&& var) {
    var.ensure_valid_get<I>();
    return get<I>(std::move(var.data));
  }

  template <size_t I>
  requires(I < sizeof...(Types)) friend constexpr std::add_pointer_t<variant_alternative_t<I, variant>> get_if(
      variant* var_ptr) noexcept {
    return (var_ptr != nullptr && var_ptr->index_ == I) ? std::addressof(get<I>(*var_ptr)) : nullptr;
  }

  template <typename T>
  requires(details::appears_once<T, Types...>) friend constexpr std::add_pointer_t<T> get_if(
      variant* var_ptr) noexcept {
    return get_if<details::find_type_v<T, Types...>>(var_ptr);
  }

  template <size_t I>
  requires(I < sizeof...(Types)) friend constexpr std::add_pointer_t<const variant_alternative_t<I, variant>> get_if(
      variant const* var_ptr) noexcept {
    return (var_ptr != nullptr && var_ptr->index_ == I) ? std::addressof(get<I>(*var_ptr)) : nullptr;
  }

  template <typename T>
  requires(details::appears_once<T, Types...>) friend constexpr std::add_pointer_t<const T> get_if(
      variant const* var_ptr) noexcept {
    return get_if<details::find_type_v<T, Types...>>(var_ptr);
  }

  template <typename T>
  requires(details::appears_once<T, Types...>) friend constexpr T& get(variant& var) {
    return get<details::find_type_v<T, Types...>>(var);
  }

  template <typename T>
  requires(details::appears_once<T, Types...>) friend constexpr T&& get(variant&& var) {
    return get<details::find_type_v<T, Types...>>(std::move(var));
  }

  template <typename T>
  requires(details::appears_once<T, Types...>) friend constexpr T const& get(variant const& var) {
    return get<details::find_type_v<T, Types...>>(var);
  }

  template <typename T>
  requires(details::appears_once<T, Types...>) friend constexpr T const&& get(variant const&& var) {
    return get<details::find_type_v<T, Types...>>(std::move(var));
  }
};

template <typename... Types>
void swap(variant<Types...>& a, variant<Types...>& b) {
  a.swap(b);
}

template <typename T, typename... Types>
requires(details::appears_once<T, Types...>) constexpr bool holds_alternative(variant<Types...> const& v) noexcept {
  return v.index() == details::find_type_v<T, Types...>;
}

struct monostate {};

constexpr std::strong_ordering operator<=>(monostate, monostate) noexcept {
  return std::strong_ordering::equal;
}
