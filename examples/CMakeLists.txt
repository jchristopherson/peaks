# Get Dependencies
find_package(ferror QUIET)
find_package(fplot QUIET)

if (NOT ferror_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        fortran_test_helper
        GIT_TAG "origin/master"
        GIT_REPOSITORY https://github.com/jchristopherson/ferror
    )
    FetchContent_MakeAvailable(ferror)
    set(FERROR_LIBRARIES ferror)
else()
    set(FERROR_LIBRARIES ferror::ferror)
endif()

if (NOT fplot_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        fortran_test_helper
        GIT_TAG "origin/master"
        GIT_REPOSITORY https://github.com/jchristopherson/fplot
    )
    FetchContent_MakeAvailable(fplot)
    set(FPLOT_LIBRARIES fplot)
else()
    set(FPLOT_LIBRARIES fplot::fplot)
endif()

# Build the examples
add_executable(peaks_example_1 peaks_example_1.f90)
target_link_libraries(
    peaks_example_1
    peaks
    ${FERROR_LIBRARIES}
    ${FPLOT_LIBRARIES}
)

if (${BUILD_SHARED_LIBS} AND WIN32)
    add_custom_command(
        TARGET peaks_example_1
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy 
        $<TARGET_RUNTIME_DLLS:peaks_example_1> 
        $<TARGET_FILE_DIR:peaks_example_1>
        COMMAND_EXPAND_LISTS
    )
endif()