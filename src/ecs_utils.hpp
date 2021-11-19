#pragma once
#include <array>
#include <cstdint>
#include <string_view>
#include <tuple>
#include <type_traits>

namespace utils {
using SizeT = std::uint_fast32_t;

// C++ type name from:
// https://stackoverflow.com/questions/81870/is-it-possible-to-prSize-a-variables-type-in-standard-c/56766138#56766138
template <typename T>
constexpr auto TypeName() {
  std::string_view name, prefix, suffix;
#ifdef __clang__
  name = __PRETTY_FUNCTION__;
  prefix = "auto utils::TypeName() [T = ";
  suffix = "]";
#elif defined(__GNUC__)
  name = __PRETTY_FUNCTION__;
  prefix = "constexpr auto utils::TypeName() [with T = ";
  suffix = "]";
#elif defined(_MSC_VER)
  name = __FUNCSIG__;
  prefix = "auto __cdecl utils::TypeName<";
  suffix = ">(void)";
#endif
  name.remove_prefix(prefix.size());
  name.remove_suffix(suffix.size());
  return name;
}

// compile time string hash for types from:
// https://roartindon.blogspot.com/2014/10/compile-time-murmur-hash-in-c.html
namespace murmur3 {
namespace detail {
template <SizeT...>
struct Sequence {};
template <SizeT N, SizeT... S>
struct CreateSequence : CreateSequence<N - 1, N - 1, S...> {};
template <SizeT... S>
struct CreateSequence<0, S...> {
  typedef Sequence<S...> Type;
};

constexpr SizeT UpdateHash1(SizeT k) { return k * 0xcc9e2d51; }
constexpr SizeT UpdateHash2(SizeT k) { return (k << 15) | (k >> (32 - 15)); }
constexpr SizeT UpdateHash3(SizeT k) { return k * 0x1b873593; }
constexpr SizeT UpdateHash4(SizeT hash, SizeT block) { return hash ^ block; }
constexpr SizeT UpdateHash5(SizeT hash) {
  return ((hash << 13) | (hash >> (32 - 13))) * 5 + 0xe6546b64;
}

constexpr SizeT UpdateHash(SizeT hash, SizeT block) {
  return UpdateHash5(
      UpdateHash4(hash, UpdateHash3(UpdateHash2(UpdateHash1(block)))));
}

constexpr SizeT UpdateLastHash(SizeT hash, SizeT block) {
  return UpdateHash4(hash, UpdateHash3(UpdateHash2(UpdateHash1(block))));
}

template <typename... C>
constexpr SizeT CalculateHashRounds(SizeT seed, C... c);

template <>
constexpr SizeT CalculateHashRounds(SizeT seed) {
  return seed;
}

template <>
constexpr SizeT CalculateHashRounds(SizeT seed, char c0) {
  return UpdateLastHash(seed, std::uint8_t(c0));
}

template <>
constexpr SizeT CalculateHashRounds(SizeT seed, char c0, char c1) {
  return UpdateLastHash(seed, std::uint8_t(c0) | std::uint8_t(c1) << 8);
}

template <>
constexpr SizeT CalculateHashRounds(SizeT seed, char c0, char c1, char c2) {
  return UpdateLastHash(
      seed, std::uint8_t(c0) | std::uint8_t(c1) << 8 | std::uint8_t(c2) << 16);
}

template <typename... C>
constexpr SizeT CalculateHashRounds(SizeT seed, char c0, char c1, char c2,
                                    char c3, C... c) {
  return CalculateHashRounds(
      UpdateHash(seed, std::uint8_t(c0) | std::uint8_t(c1) << 8 |
                           std::uint8_t(c2) << 16 | std::uint8_t(c3) << 24),
      c...);
}

constexpr SizeT CalculateFinalHash1(SizeT h, SizeT length) {
  return h ^ length;
}

constexpr SizeT CalculateFinalHash2(SizeT h) { return h ^ (h >> 16); }

constexpr SizeT CalculateFinalHash3(SizeT h) { return h * 0x85ebca6b; }

constexpr SizeT CalculateFinalHash4(SizeT h) { return h ^ (h >> 13); }

constexpr SizeT CalculateFinalHash5(SizeT h) { return h * 0xc2b2ae35; }

constexpr SizeT CalculateFinalHash6(SizeT h) { return h ^ (h >> 16); }

constexpr SizeT CalculateFinalHash(SizeT h, SizeT length) {
  return CalculateFinalHash6(
      CalculateFinalHash5(CalculateFinalHash4(CalculateFinalHash3(
          CalculateFinalHash2(CalculateFinalHash1(h, length))))));
}

// This is used to convert from calling const char (&s)[N]
// To CalculateHashRounds(seed, s[0], s[1], s[2], s[3], ... )
template <SizeT N, SizeT... S>
constexpr SizeT Unpack(unsigned seed, const char (&s)[N], Sequence<S...>) {
  return CalculateHashRounds(seed, s[S]...);
}
// This is used to convert from calling std::string_view
// To CalculateHashRounds(seed, s[0], s[1], s[2], s[3], ... )
template <SizeT N, SizeT... S>
constexpr SizeT Unpack(unsigned seed, const std::string_view& s,
                       Sequence<S...>) {
  return CalculateHashRounds(seed, s[S]...);
}

template <SizeT N>
constexpr SizeT murmur3_32(const char (&s)[N], SizeT seed = 0) {
  return CalculateFinalHash(
      Unpack(seed, s, typename CreateSequence<N - 1>::Type()), N - 1);
}

template <SizeT N>
constexpr SizeT murmur3_32(const std::string_view& s, SizeT seed = 0) {
  return CalculateFinalHash(
      Unpack<N>(seed, s, typename CreateSequence<N>::Type()), N);
}

}  // namespace detail

template <typename T>
constexpr auto Hash32TypeName() {
  constexpr auto s = TypeName<T>();
  return detail::murmur3_32<s.length()>(std::move(s));
}

}  // namespace murmur3

// tuple utilities to filter on its contained types
namespace tuple {

namespace detail {

// slice from:
// https://noassemblyrequired.net/2021/10/18/slicing-stdtuple/
template <typename tuple, SizeT Offs, SizeT... indices>
constexpr auto Slice_Impl(tuple const& t, std::index_sequence<indices...>) {
  return std::make_tuple(std::get<indices + Offs>(t)...);
}
}  // namespace detail

template <SizeT Cnt, SizeT Offs, typename... Args>
constexpr auto Slice(std::tuple<Args...> const& tuple) {
  if constexpr (sizeof...(Args) == 0 || sizeof...(Args) < Cnt + Offs) {
    return std::tuple<>();
  } else {
    return detail::Slice_Impl<std::tuple<Args...>, Offs>(
        tuple, std::make_index_sequence<Cnt>{});
  }
};

// remove type from tuple
template <typename R, bool remove_duplicate, bool done = false,
          typename Tup = std::tuple<>>
constexpr auto Remove(std::tuple<>, Tup t = Tup()) {
  return t;
}

template <typename R, bool remove_duplicate, bool done = false,
          typename Tup = std::tuple<>, typename T, typename... Ts>
constexpr auto Remove(std::tuple<T, Ts...> t_src, Tup t = Tup()) {
  if constexpr (!done && std::is_same_v<R, T>) {
    if constexpr (remove_duplicate) {
      return Remove<R, remove_duplicate>(Slice<sizeof...(Ts), 1>(t_src), t);
    } else {
      return Remove<R, remove_duplicate, true>(Slice<sizeof...(Ts), 1>(t_src),
                                               t);
    }
  } else {
    return Remove<R, remove_duplicate>(
        std::tuple<Ts...>(Slice<sizeof...(Ts), 1>(t_src)),
        std::tuple_cat(t, std::tuple<T>(std::get<0>(t_src))));
  }
}

// checks of tuple contais a type
template <typename U, typename... T>
constexpr bool Contains() {
  return std::disjunction_v<std::is_same<T, U>...>;
}

namespace detail {
namespace detail {
template <typename RAIt>
constexpr RAIt next(
    RAIt it, typename std::iterator_traits<RAIt>::difference_type n = 1) {
  return it + n;
}

template <typename RAIt>
constexpr auto distance(RAIt first, RAIt last) {
  return last - first;
}

template <class ForwardIt1, class ForwardIt2>
constexpr void iter_swap(ForwardIt1 a, ForwardIt2 b) {
  auto temp = std::move(*a);
  *a = std::move(*b);
  *b = std::move(temp);
}

template <class InputIt, class UnaryPredicate>
constexpr InputIt find_if_not(InputIt first, InputIt last, UnaryPredicate q) {
  for (; first != last; ++first) {
    if (!q(*first)) {
      return first;
    }
  }
  return last;
}

template <class ForwardIt, class UnaryPredicate>
constexpr ForwardIt partition(ForwardIt first, ForwardIt last,
                              UnaryPredicate p) {
  first = detail::find_if_not(first, last, p);
  if (first == last) return first;

  for (ForwardIt i = detail::next(first); i != last; ++i) {
    if (p(*i)) {
      detail::iter_swap(i, first);
      ++first;
    }
  }
  return first;
}

}  // namespace detail

// for static asserion
template <typename T>
static constexpr bool always_false = false;

template <class RAIt, class Compare = std::less<>>
constexpr void QuickSort(RAIt first, RAIt last, Compare cmp = Compare{}) {
  auto const N = detail::distance(first, last);
  if (N <= 1) return;
  auto const pivot = *detail::next(first, N / 2);
  auto const middle1 = detail::partition(
      first, last, [=](auto const& elem) { return cmp(elem, pivot); });
  auto const middle2 = detail::partition(
      middle1, last, [=](auto const& elem) { return !cmp(pivot, elem); });
  QuickSort(first, middle1,
            cmp);  // assert(std::is_sorted(first, middle1, cmp));
  QuickSort(middle2, last,
            cmp);  // assert(std::is_sorted(middle2, last, cmp));
}

template <class Key, class Value>
struct Pair {
  Key index;
  Value new_index;
};

template <class Key, class Value, template <typename, typename> class... Pairs>
class CompileTimeMap {
 public:
  constexpr CompileTimeMap(const Pairs<Key, Value>&... key_val)
      : pairs{key_val...} {}
  constexpr Value operator[](Key key) const { return Get(key); }
  constexpr Value& operator[](Key key) { return Get(key); }

 private:
  template <size_t n = 0>
  constexpr Value& Get(Key key) {
    if constexpr (n >= sizeof...(Pairs)) {
      (void)key;
      // TODO: get a better solution
      // static_assert(always_false<decltype(n)>, "Wrong key!");
      return const_cast<Value&>(KeyNotFound);
    } else {
      return pairs[n].index == key ? pairs[n].new_index : Get<n + 1>(key);
    }
  }

  constexpr Value Get(Key key, SizeT i = 0) const {
    return i == sizeof...(Pairs)
               ? KeyNotFound
               : pairs[i].index == key ? pairs[i].new_index : Get(key, i + 1);
  }

  static constexpr Value KeyNotFound = -1;

 public:
  std::array<Pair<Key, Value>, sizeof...(Pairs)> pairs;
};

template <typename... Ts, size_t... idx>
constexpr auto SortTupleTypesSizeImpl(std::index_sequence<idx...>) {
  constexpr auto size = sizeof...(Ts);

  CompileTimeMap ctMap(Pair<SizeT, SizeT>{idx, sizeof(Ts)}...);

  QuickSort(
      ctMap.pairs.begin(), ctMap.pairs.end(),
      [](const auto& a, const auto& b) { return a.new_index > b.new_index; });

  // assign mapping
  for (SizeT i = 0; i < size; ++i) {
    ctMap.pairs[i].new_index = i;
  }

  return ctMap;
}

template <typename... Ts>
constexpr auto SortTupleTypesSize() {
  return SortTupleTypesSizeImpl<Ts...>(
      std::make_index_sequence<sizeof...(Ts)>{});
}
}  // namespace detail

template <typename... Ts>
struct SortedTuple;
namespace detail {
template <typename T, bool remove_duplicate, typename Tuple>
constexpr auto RemoveFactory(Tuple t) {
  return SortedTuple(utils::tuple::Remove<T, remove_duplicate>(t));
}
}  // namespace detail

// sorted tuple to reduce memory
template <typename... Ts>
struct SortedTuple {
  constexpr SortedTuple() { SortedTuple(std::tuple<Ts...>()); }
  constexpr SortedTuple(const std::tuple<Ts...>& tuple)
      : tuple_(MakeSortedTuple(std::make_index_sequence<sizeof...(Ts)>{},
                               tuple)) {}
  constexpr SortedTuple(const SortedTuple<Ts...>& tuple)
      : tuple_(tuple.tuple_) {}
  constexpr SortedTuple(SortedTuple<Ts...>&& tuple)
      : tuple_(std::move(tuple.tuple_)) {}

  constexpr auto& operator=(const SortedTuple<Ts...>& tuple) {
    tuple_ = tuple.tuple_;
    return *this;
  }

  template <size_t n = 0>
  constexpr auto& Get() {
    constexpr auto map = detail::SortTupleTypesSize<Ts...>();
    if constexpr (n < sizeof...(Ts)) {
      return std::get<map[n]>(tuple_);
    } else {
      static_assert(detail::always_false<decltype(n)>, "Out of bounds access");
      return std::get<0>(tuple_);
    }
  }

  template <size_t n = 0>
  constexpr const auto& Get() const {
    constexpr auto map = detail::SortTupleTypesSize<Ts...>();
    if constexpr (n < sizeof...(Ts)) {
      return std::get<map[n]>(tuple_);
    } else {
      static_assert(detail::always_false<decltype(n)>, "Out of bounds access");
      return std::get<0>(tuple_);
    }
  }

  template <typename T>
  constexpr auto& Get() {
    static_assert(HasType<T>(), "Type isn't contained!");
    return std::get<T>(tuple_);
  }

  template <typename T>
  constexpr const auto& Get() const {
    static_assert(HasType<T>(), "Type isn't contained!");
    return std::get<T>(tuple_);
  }

  template <typename T, typename... Args>
  void Set(Args... args) {
    if constexpr (std::is_constructible_v<T, Args...>) {
      Get<T>() = T(args...);
    } else {
      Get<T>() = T{args...};
    }
  }

  template <typename T>
  static constexpr bool Has() {
    return HasType<T>();
  }

  template <typename T>
  static constexpr bool HasType() {
    return utils::tuple::Contains<T, Ts...>();
  }

  template <typename T>
  constexpr auto AddType() const {
    return SortedTuple<Ts..., T>(std::tuple_cat(tuple_, std::tuple<T>()));
  }

  template <typename T>
  constexpr auto RemoveType() const {
    auto tmp = std::tuple<Ts...>(std::get<Ts>(tuple_)...);
    return detail::RemoveFactory<T, false>(tmp);
  }

  template <typename T>
  constexpr auto RemoveTypes() const {
    auto tmp = std::tuple<Ts...>(std::get<Ts>(tuple_)...);
    return detail::RemoveFactory<T, true>(tmp);
  }

 private:
  template <size_t... idx>
  static constexpr auto MakeSortedTuple(std::index_sequence<idx...>,
                                        std::tuple<Ts...> tup) {
    constexpr auto map = detail::SortTupleTypesSize<Ts...>();
    return std::tuple<
        std::tuple_element_t<map.pairs[idx].index, std::tuple<Ts...>>...>(
        std::get<map.pairs[idx].index>(tup)...);
  }

  using TupleType = decltype(MakeSortedTuple(
      std::make_index_sequence<sizeof...(Ts)>{}, std::tuple<Ts...>()));

  template <size_t n = 0>
  constexpr void CopyTuple(std::tuple<Ts...> tuple) {
    constexpr auto map = detail::SortTupleTypesSize<Ts...>();
    if constexpr (n < sizeof...(Ts)) {
      std::get<n>(tuple_) = std::get<map.pairs[n].index>(tuple);
      CopyTuple<n + 1>(tuple);
    }
  }

  TupleType tuple_;
};

// checks of tuple contais a type
template <typename... T>
constexpr bool AreDistinctTypes() {
  auto map = detail::CompileTimeMap(
      detail::Pair<SizeT, SizeT>{murmur3::Hash32TypeName<T>(), 0}...);
  bool distinct = true;
  auto f = [&](SizeT x) { map[x] += 1; };
  (f(murmur3::Hash32TypeName<T>()), ...);
  auto f_check = [&](SizeT x) {
    if (map[x] > 1) {
      distinct = false;
    }
  };
  (f_check(murmur3::Hash32TypeName<T>()), ...);
  return distinct;
}
}  // namespace tuple

// See http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4502.pdf.
template <typename...>
using Void = void;

// Primary template handles all types not supporting the operation.
template <typename, template <typename> class, typename = Void<>>
struct Detect : std::false_type {};

// Specialization recognizes/validates only types supporting the archetype.
template <typename T, template <typename> class Op>
struct Detect<T, Op, Void<Op<T>>> : std::true_type {};

template <class... Ts>
struct OverLoad : Ts... {
  OverLoad(Ts...) = delete;
  using Ts::operator()...;
};  // example: OverLoad{[](Size x) {}, [](...) {}};

}  // namespace utils
