cmake_minimum_required(VERSION 3.0.0)

project(ECS)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# ccache
find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
endif(CCACHE_FOUND)

# ##############################################################################
# global flags and directories setup
# ##############################################################################

# directories - everything goes in the same place
set(OUTPUT_DIR ${PROJECT_BINARY_DIR}/bin/)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${OUTPUT_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${OUTPUT_DIR})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${OUTPUT_DIR})

# latest c++ standards
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  set(CMAKE_CXX_FLAGS
      "${CMAKE_CXX_FLAGS} -ggdb3 -O3 -std=c++17 -fvisibility=hidden")
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++latest")
endif()

# ##############################################################################
# the main executable
# ##############################################################################

add_executable(
  example_app
  # sources
  src/example.cpp
  src/ecs.hpp
  src/ecs_utils.hpp)

# add an include dir for ease of use
target_include_directories(example_app PUBLIC src)

# enable warnings
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  target_compile_options(example_app PRIVATE -Wall -Wextra)
else()
  target_compile_options(example_app PRIVATE /W4)
endif()


####################################################################################################
# tests
####################################################################################################
install(TARGETS example_app)

enable_testing()
add_test(NAME example_app COMMAND example_app)
