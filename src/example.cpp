#include <iostream>
#include <string>

#include "ecs.hpp"

// COMPONENTS
struct Position {
  float x, y;
};

struct Direction {
  float angle;
};

struct Mesh {
  std::string name;
};

// Archetypes
using Physics = ecs::Archetype<Position, Direction>;
using Render = ecs::Archetype<Position, Mesh>;

// example
int main(int, char*[]) {
  auto world = ecs::ECS<Physics, Render>();
  auto e1 = world.Add<Physics>();  // new entity from physics Archetype

  e1.Set<Position>(1.4f, 1.134f);

  auto e2 = world.Add<Render>();
  e2.Set<Position>(200.24f, 100.0f);
  e2.Set<Mesh>("ECS Rendering!");

  // loop over physics entities
  world.Run<Physics>([](auto& e) {
    auto position = e.template Get<Position>();
    auto direction = e.template Get<Direction>();
    std::cout << position.x << std::endl;
    std::cout << position.y << std::endl;
    std::cout << direction.angle << std::endl;
    e.template Set<Position>(128.0231f, 1231.0f / 0.0f);
  });

  // loop over all entities
  world.Run([](auto e) {
    auto position = e.template Get<Position>();
    std::cout << position.x << std::endl;
    std::cout << position.y << std::endl;
    // fail compilation as direction isn't shared among all entities
    // auto direction = e.template Get<Direction>();
    if constexpr (e.template Has<Direction>()) {
      auto direction = e.template Get<Direction>();
      std::cout << direction.angle << std::endl;
    }

    if constexpr (e.template Has<Mesh>()) {
      auto mesh = e.template Get<Mesh>();
      std::cout << mesh.name << std::endl;
    }

    // you could use utils::OverLoad aswell
  });

  return 0;
}
