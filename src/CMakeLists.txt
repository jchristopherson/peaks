# Locate the source directory
set(dir ${CMAKE_CURRENT_SOURCE_DIR})

# Define the source files
set(FSTATS_SOURCES
    ${dir}/peaks.f90
)
set(PEAKS_SOURCES ${PEAKS_SOURCES} PARENT_SCOPE)