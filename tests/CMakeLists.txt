set(peaks_test_sources
    peaks_tests.f90
    peaks_tests_implementation.f90
)

add_executable(peaks_tests ${peaks_test_sources})
target_link_libraries(peaks_tests peaks)
add_test(
    NAME peaks_tests
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:peaks_tests>
)