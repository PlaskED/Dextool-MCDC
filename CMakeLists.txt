cmake_minimum_required(VERSION 2.8)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake" "${CMAKE_SOURCE_DIR}/cmake/Modules")

project(deXtool)
enable_language(D)

include(common)
if(NOT LIBCLANG_LIB_PATH OR NOT LIBCLANG_LDFLAGS)
    include(FindLibClang)
endif()
include(BuildD)

#
# Main configuration.
#

set(BUILD_TEST false CACHE BOOL "true to build and run unittests")
if(BUILD_TEST)
    enable_testing()
endif()

# Generally, we want to install everything into CMAKE_INSTALL_PREFIX, but when
# it is /usr, put the config files into /etc to meet common practice.
if(NOT DEFINED SYSCONF_INSTALL_DIR)
    if(CMAKE_INSTALL_PREFIX STREQUAL "/usr")
        set(SYSCONF_INSTALL_DIR "/etc")
    else()
        set(SYSCONF_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/etc")
    endif()
endif()

file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR})

# Setup D compiler flags (DMD syntax, which also works with LDMD).
set(DDMD_DFLAGS "-w -wi -vcolumns")
set(DDMD_LFLAGS "")
set(DDMD_COV_FLAG "")

# for multi-config builds, these options have to be added later to the custom command
if(CMAKE_BUILD_TYPE MATCHES "Debug")
    append("-g -debug" DDMD_DFLAGS)
    if("${D_COMPILER_ID}" STREQUAL "LDMD")
        append("-link-debuglib" DDMD_DFLAGS)
    endif()
elseif(CMAKE_BUILD_TYPE MATCHES "RelWithDebInfo")
    append("-g -O -inline -release" DDMD_DFLAGS)
else()
    # Default to a Release build type
    append("-inline -release" DDMD_DFLAGS)
    if("${D_COMPILER_ID}" STREQUAL "LDMD")
        append("-enable-inlining -O5" DDMD_DFLAGS)
    else()
        append("-O" DDMD_DFLAGS)
    endif()
endif()

#
# Resources
#
append("-J${PROJECT_BINARY_DIR}/resources" DDMD_DFLAGS) # Needed for importing text files
# strip leading/trailing whitespace
string(STRIP "${DDMD_DFLAGS}" DDMD_DFLAGS)

#
# Emedded version
#
set(DEXTOOL_EMBEDDED_VERSION_PATH ${PROJECT_BINARY_DIR}/resources/version.txt)

# Generate the version description
execute_process(
    COMMAND ${PROJECT_SOURCE_DIR}/gen_version_from_git.sh
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)

add_custom_target(dextool_embedded_version
    DEPENDS
    ${DEXTOOL_EMBEDDED_VERSION_PATH}
)

#
# Configure the main executable that all underlying plugins adher to
#
set(DEXTOOL_MAIN_EXE "dextool")
if(CMAKE_BUILD_TYPE MATCHES "Debug")
    set(DEXTOOL_MAIN_EXE "${DEXTOOL_MAIN_EXE}_debug")
endif()
set(DEXTOOL_MAIN_EXE_FULL ${PROJECT_BINARY_DIR}/${DEXTOOL_MAIN_EXE})

#
# Distribute building
#

# base libraries used by binaries
include(dextool_clang.cmake)
include(dextool_cpptooling.cmake)
include(dextool_dextool.cmake)
include(dextool_dsrcgen.cmake)
include(dextool_libclang.cmake)

if(BUILD_TEST)
    include(dextool_devtool.cmake)
    include(dextool_unit_threaded.cmake)
    add_subdirectory(source/test)
    add_subdirectory(test)
endif()

# binary
add_subdirectory(source)
add_subdirectory(plugin)

#
# Install target.
#

#
# Packaging
#

#include (CMakeCPack.cmake)
#include (CPack)