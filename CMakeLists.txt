# Master CMAKE Build Script
cmake_minimum_required(VERSION 3.24)
project(
    peaks
    LANGUAGES Fortran
    VERSION 1.0.0
)

# Get helper macros and functions
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

# Confgiure everything
add_subdirectory(configure)

# Source
add_subdirectory(src)
add_fortran_library(
    ${PROJECT_NAME}
    ${PROJECT_INCLUDE_DIR}
    ${CMAKE_INSTALL_INCLUDEDIR}
    ${PROJECT_VERSION}
    ${PROJECT_VERSION_MAJOR}
    ${PEAKS_SOURCES}
)

# Installation
add_subdirectory(install)

# Testing
option(BUILD_TESTING "Build tests")
include(CTest)
message(STATUS "Build tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()

# Examples
option(BUILD_PEAKS_EXAMPLES "Build PEAKS examples")
message(STATUS "Build PEAKS examples: ${BUILD_PEAKS_EXAMPLES}")
if (BUILD_PEAKS_EXAMPLES)
    add_subdirectory(examples)
endif()