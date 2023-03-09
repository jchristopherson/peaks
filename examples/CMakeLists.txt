include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

# Get FPLOT
add_subdirectory(fplot)

# Build the examples
add_executable(peaks_example_1 peaks_example_1.f90)
link_library(peaks_example_1 peaks ${PROJECT_INCLUDE_DIR})
link_library(peaks_example_1 ${fplot_LIBRARY} ${fplot_INCLUDE_DIR})