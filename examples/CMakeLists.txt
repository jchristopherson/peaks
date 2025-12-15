# Get FPLOT
add_subdirectory(fplot)

# Build the examples
add_executable(peaks_example_1 peaks_example_1.f90)
target_link_libraries(
    peaks_example_1
    peaks
    ${fplot_LIBRARY}
)
