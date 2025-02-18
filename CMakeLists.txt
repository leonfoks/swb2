cmake_minimum_required(VERSION 3.1)

# CMake project for swb2

message("Processing top-level CMakelists.txt for project swb2")

set(SWB_MAJOR_VERSION "2")
set(SWB_MINOR_VERSION "0")

#------------------------------------------------------------------------------------#
#                           Build directory check                                    #
#------------------------------------------------------------------------------------#
if(${CMAKE_SOURCE_DIR}/src STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "Cannot be built in the source directory. Use out-of-source build instead.")
  message(FATAL_ERROR "  cd /some/work/dir/build")
  message(FATAL_ERROR "  cmake -i /path/to/source")
endif()

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
#
project (swb2 Fortran C)
enable_language (Fortran C)

SET(CPACK_GENERATOR "TGZ")
include(CPack)

# Add our local modules to the module path
if(NOT CMAKE_MODULE_PATH)
  set(CMAKE_MODULE_PATH)
endif()
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_fortran/cmake")

set(CMAKE_INCLUDE_CURRENT_DIR ON)

include(${CMAKE_SOURCE_DIR}/cmake_fortran/cmake/FortranEnvironment.cmake)


#set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ".so" ${CMAKE_FIND_LIBRARY_SUFFIXES})

include( ${CMAKE_SOURCE_DIR}/cmake/version_control.cmake )
# include( ${CMAKE_MODULE_PATH}/zlib.cmake )
include( ${CMAKE_SOURCE_DIR}/cmake_fortran/cmake/hdf5.cmake )
include( ${CMAKE_SOURCE_DIR}/cmake_fortran/cmake/netcdf4.cmake )

#if(MSVC)
  # Automatically adds compiler definitions to all subdirectories too.
  add_definitions(/D_CRT_SECURE_NO_DEPRECATE /DWIN32_LEAN_AND_MEAN)
  # # Disable all warnings
  # string (REPLACE "/W3" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
  # set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /w")
#endif(MSVC)

#link_libraries( "-static" )
include_directories( ${CMAKE_SOURCE_DIR}/src ${PROJECT_SOURCE_DIR}/src/proj4 )

include( cmake/print_summary.cmake )

add_subdirectory(src)
# add_subdirectory(test/unit_tests)

if ( CMAKE_HOST_WIN32 )
  set( OUTPUT_BIN_TARGET_DIR win_x86 )
elseif( CMAKE_HOST_APPLE )
  set( OUTPUT_BIN_TARGET_DIR macOS )
elseif( CMAKE_HOST_UNIX AND (NOT CMAKE_HOST_APPLE) )
  set( OUTPUT_BIN_TARGET_DIR linux_x64 )
endif()

if(CMAKE_Fortran_COMPILER_ID=="GNU")
  set( OUTPUT_BIN_TARGET_DIR ${OUTPUT_BIN_TARGET_DIR}/gfortran)
elseif( CMAKE_Fortran_COMPILER_ID=="PGI")

elseif(CMAKE_Fortran_COMPILER_ID=="GNU")

endif()

# prepend location of project 'bin' directory
set( OUTPUT_BIN_TARGET_DIR ${PROJECT_SOURCE_DIR}/bin/${OUTPUT_BIN_TARGET_DIR} )

if ( WIN32 )
  add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb2.exe ${OUTPUT_BIN_TARGET_DIR}/swb2.exe
                #  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats2.exe ${OUTPUT_BIN_TARGET_DIR}/swbstats2.exe
		            #  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/test/unit_tests/swbtest.exe ${PROJECT_SOURCE_DIR}/test/unit_tests/swbtest.exe
  )
else()
  add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb2 ${OUTPUT_BIN_TARGET_DIR}/swb2
                #  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats2 ${OUTPUT_BIN_TARGET_DIR}/swbstats2
                #  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/test/unit_tests/swbtest ${PROJECT_SOURCE_DIR}/test/unit_tests/swbtest
  )
endif()

file(WRITE VERSION "${SWB_MAJOR_VERSION}.${SWB_MINOR_VERSION}, build ${GIT_COMMITS_ON_BRANCH}")
