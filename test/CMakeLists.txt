include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

# Get FORTRAN_TEST_HELPER
add_subdirectory(fortran_test_helper)

# Build the tests
set(peaks_test_sources
    peaks_tests.f90
    peaks_tests_implementation.f90
)

add_executable(peaks_tests ${peaks_test_sources})
link_library(peaks_tests peaks ${PROJECT_INCLUDE_DIR})
link_library(peaks_tests ${fortran_test_helper_LIBRARY} ${fortran_test_helper_INCLUDE_DIR})
add_test(
    NAME peaks_tests
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:peaks_tests>
)