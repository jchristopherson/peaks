# Locate the source directory
set(dir ${CMAKE_CURRENT_SOURCE_DIR})

# Define the source files
set(PEAKS_SOURCES
    ${dir}/peaks.f90
    ${dir}/peaks_implementation.f90
)
set(PEAKS_SOURCES ${PEAKS_SOURCES} PARENT_SCOPE)