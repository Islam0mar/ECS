#pragma once

#include <functional>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <variant>
#include <vector>

#include "ecs_utils.hpp"

namespace ecs {
using SizeT = std::uint_fast32_t;

template <typename Archetype, typename ECS>
struct Entity {
  constexpr Entity(SizeT n = utils::counter::Generate(), ECS* ecs = nullptr)
      : n_(n), ecs_(ecs) {}

  template <typename T>
  static constexpr bool Has() {
    return Archetype::template HasComponent<T>();
  }

  template <typename T>
  T& Get() {
    return ecs_->template GetArchetype<Archetype>()
        .GetComponent(n_)
        .template Get<T>();
  }

  template <typename T>
  const T& Get() const {
    return ecs_->template GetArchetype<Archetype>()
        .GetComponent(n_)
        .template Get<T>();
  }

  template <typename C, typename... Args>
  void Set(Args... args) {
    ecs_->template GetArchetype<Archetype>().GetComponent(n_).template Set<C>(
        args...);
  }

  SizeT Index() const { return n_; }

 private:
  const SizeT n_;
  ECS* const ecs_;
};

template <typename...>
struct Archetype;
namespace detail {
template <typename R, typename Tuple>
constexpr auto RemoveFactory(Tuple t) {
  return Archetype(t.template RemoveType<R>());
}
}  // namespace detail
template <typename... Components>
struct Archetype {
  using Type = Archetype<Components...>;
  using ComponentTuple = utils::tuple::SortedTuple<Components...>;
  using ComponentTupleVector = std::vector<ComponentTuple>;
  using Iterator = typename ComponentTupleVector::iterator;
  using ConstIterator = typename ComponentTupleVector::const_iterator;

  static_assert(utils::tuple::AreDistinctTypes<Components...>(),
                "Repeated Components!");

  Iterator begin() { return components_.begin(); }
  Iterator end() { return components_.end(); }
  ConstIterator cbegin() { return components_.cbegin(); }
  ConstIterator cend() { return components_.cend(); }

  constexpr Archetype() : components_() {}
  constexpr Archetype(const std::variant<Components...>&)
      : components_(ComponentTupleVector()) {}

  constexpr Archetype(const ComponentTupleVector& components)
      : components_(components) {}
  constexpr Archetype(const Archetype& archetype)
      : components_(archetype.components_) {}
  constexpr Archetype(Archetype&& archetype)
      : components_(std::move(archetype.components_)) {}
  constexpr Archetype(const ComponentTuple&) : components_() {}

  constexpr auto AddEntity() {
    auto e = components_.size();
    components_.push_back(ComponentTuple());
    return e;
  }

  constexpr auto& GetComponent(SizeT idx) { return components_[idx]; }
  constexpr const auto& GetComponent(SizeT idx) const {
    return components_[idx];
  }

  template <typename T>
  static constexpr bool HasComponent() {
    return ComponentTuple::template HasType<T>();
  }

  template <typename T>
  constexpr auto AddComponent() const {
    static_assert(HasComponent<T>() == false, "Component is already added!");
    return Archetype<Components..., T>(ComponentTuple().template AddType<T>());
  }

  template <typename T>
  constexpr auto RemoveComponent() const {
    static_assert(HasComponent<T>(), "Component isn't added!");
    return detail::RemoveFactory<T>(ComponentTuple());
  }

  // private:
  ComponentTupleVector components_;
};

// messages, mlti, sync
template <typename... Archetypes>
struct ECS {
  using Type = ECS<Archetypes...>;
  using ArchetypeTuple = utils::tuple::SortedTuple<Archetypes...>;

  static_assert(utils::tuple::AreDistinctTypes<Archetypes...>(),
                "Repeated Archetypes!");

  // for each Archetypes
  template <typename F, typename... Args>
  void CustomRun(F f, Args... args) {
    (std::invoke(f, args..., GetArchetype<Archetypes>()), ...);
  }

  // for each Archetypes
  template <typename F, typename... Args>
  void RunReadOnly(F f, Args... args) {
    (RunReadOnly<Archetypes>(f, args...), ...);
  }

  // for each component in an Archetype
  template <typename Archetype, typename F, typename... Args>
  void RunReadOnly(F f, Args... args) {
    for (const auto& e : archetypes_.template Get<Archetype>()) {
      std::invoke(f, args..., e);
    }
  }

  // for each Archetypes
  template <typename F, typename... Args>
  void Run(F f, Args... args) {
    (Run<Archetypes>(f, args...), ...);
  }

  // for each component in an Archetype
  template <typename Archetype, typename F, typename... Args>
  void Run(F f, Args... args) {
    for (auto& e : archetypes_.template Get<Archetype>()) {
      std::invoke(f, args..., e);
    }
  }

  constexpr ECS(const std::variant<Archetypes...>&) : archetypes_() {}

  constexpr ECS() : archetypes_() {}

  template <typename T>
  constexpr auto Add() {
    static_assert(utils::tuple::Contains<T, Archetypes...>(),
                  "Archetype is not registered!");
    auto id = archetypes_.template Get<T>().AddEntity();

    return Entity<T, Type>(id, this);
  }

  template <typename T>
  constexpr auto& GetArchetype() {
    static_assert(utils::tuple::Contains<T, Archetypes...>(),
                  "Archetype is not registered!");
    return archetypes_.template Get<T>();
  }

  template <typename T>
  constexpr const auto& GetArchetype() const {
    static_assert(utils::tuple::Contains<T, Archetypes...>(),
                  "Archetype is not registered!");
    return archetypes_.template Get<T>();
  }

 private:
  ArchetypeTuple archetypes_;
};
}  // namespace ecs
