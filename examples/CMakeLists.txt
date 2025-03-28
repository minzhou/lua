cmake_minimum_required(VERSION 3.12)
project(TestProject)

# C++标准配置
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# 编译器选项
if (CMAKE_COMPILER_IS_GNUCXX)
    add_compile_options(-D_GLIBCXX_USE_CXX11_ABI=0)
else()
    add_compile_options(-D_GLIBCXX_DEBUG)
endif()

# 可执行目标
configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/test.lua
        ${CMAKE_CURRENT_BINARY_DIR}/test.lua
        COPYONLY
)

add_executable(test_main ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp)
set_target_properties(test_main PROPERTIES LINKER_LANGUAGE CXX)

target_include_directories(test_main PUBLIC ${lua_INCLUDE_DIR})
target_link_libraries(test_main lua)

# Linux静态链接配置
if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    target_link_options(test_main PRIVATE
            -static-libstdc++
            -static-libgcc
    )
endif()