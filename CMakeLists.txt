cmake_minimum_required(VERSION 3.12)
project(lua C ASM)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(LUA_LIB_SRCS
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lapi.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lcode.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lctype.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/ldebug.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/ldo.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/ldump.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lfunc.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lgc.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/llex.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lmem.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lobject.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lopcodes.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lparser.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lstate.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lstring.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/ltable.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/ltm.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lundump.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lvm.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lzio.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lauxlib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lbaselib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lbitlib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lcorolib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/ldblib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/liolib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lmathlib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/loslib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lstrlib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/ltablib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lutf8lib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/loadlib.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/linit.c
        ${CMAKE_CURRENT_SOURCE_DIR}/src/luac.c)
set(LUA_LIB_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/src)
add_library(${PROJECT_NAME} STATIC ${LUA_LIB_SRCS})
target_include_directories(${PROJECT_NAME} PUBLIC ${LUA_LIB_INCLUDE})

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    list(APPEND LUA_COMPILER_DEFINES LUA_USE_MACOSX)
elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    list(APPEND LUA_COMPILER_DEFINES LUA_USE_LINUX)
    target_link_libraries(${PROJECT_NAME} dl)
elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    #list(APPEND LUA_COMPILER_DEFINES LUA_USE_WINDOWS)
    target_link_libraries(${PROJECT_NAME} dl)
endif()

SET(CMAKE_CXX_FLAGS_DEBUG "$ENV{CXXFLAGS} -O0 -Wall -g")
SET(CMAKE_CXX_FLAGS_RELEASE "$ENV{CXXFLAGS} -O3 -Wall")
target_compile_definitions(${PROJECT_NAME} PUBLIC ${LUA_COMPILER_DEFINES})

set_target_properties(
        ${PROJECT_NAME}
        PROPERTIES INTERFACE_INCLUDE_DIRECTORIES
        "${CMAKE_CURRENT_SOURCE_DIR}/src"
)