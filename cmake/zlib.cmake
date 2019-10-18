if ("$ENV{ZLIB_ROOT}" STREQUAL "")
    message(FATAL_ERROR "Could not find zlib library.  Please create an environment variable ZLIB_ROOT.")
else()
    set(ZLIB_ROOT "$ENV{ZLIB_ROOT}" CACHE INTERNAL "Get the zlib install directory")
    message("Using ZLIB library installed at ${ZLIB_ROOT}")

    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -I${ZLIB_ROOT}/include")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -L${ZLIB_ROOT}/lib -llibz")
endif()
