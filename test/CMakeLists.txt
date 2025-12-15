# Dependencies
find_package(fortran_test_helper QUIET)
if (NOT fortran_test_helper_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        fortran_test_helper
        GIT_TAG "origin/main"
        GIT_REPOSITORY https://github.com/jchristopherson/fortran_test_helper
    )
    FetchContent_MakeAvailable(fortran_test_helper)
    set(FTH_LIBRARIES fortran_test_helper)
else()
    set(FTH_LIBRARIES fortran_test_helper::fortran_test_helper)
endif()

# Build the tests
set(peaks_test_sources
    peaks_tests.f90
    peaks_tests_implementation.f90
)

add_executable(peaks_tests ${peaks_test_sources})
target_link_libraries(
    peaks_tests
    peaks
    ${FTH_LIBRARIES}
)
add_test(
    NAME peaks_tests
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:peaks_tests>
)

if (${BUILD_SHARED_LIBS} AND WIN32)
    add_custom_command(
        TARGET peaks_tests
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy 
        $<TARGET_RUNTIME_DLLS:peaks_tests> 
        $<TARGET_FILE_DIR:peaks_tests>
        COMMAND_EXPAND_LISTS
    )
endif()
