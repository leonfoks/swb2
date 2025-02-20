@echo off
:: remove existing Cmake cache and directories
del /F /Q CMakeCache.*
rmdir /S /Q CMakeFiles
rmdir /S /Q src
rmdir /S /Q Win32
rmdir /S /Q Debug
rmdir /S /Q Testing
rmdir /S /Q Release
rmdir /S /Q tests
rmdir /S /Q .vs
del /S /Q *.txt
del /S /Q *.sln
del /S /Q *.vcxproj*
del /S /Q *.cmake

:: set CMAKE-related and build-related variables
set CMAKEROOT=C:\Program Files\CMake\
set Fortran_COMPILER_NAME=ifort
set CMAKE_C_COMPILER=icl
set VERBOSE=0
set MAKE_EXECUTABLE_NAME=nmake

:: define other variables for use in the CMakeList.txt file
:: options are "Release", "Profile" or "Debug"
set BUILD_TYPE="Release"

:: define platform and compiler specific compilation flags
set CMAKE_Fortran_FLAGS_DEBUG="/Od /fpp /MTd /debug:full /Z7 /check:bounds,pointers,stack /traceback /free /Qzero /heap-arrays:128"
set CMAKE_Fortran_FLAGS_PROFILE="/Od /fpp /MTd /debug:full /Z7 /check:bounds,pointers,stack /traceback /free /Qzero /heap-arrays:128"
set CMAKE_Fortran_FLAGS_RELEASE="/O2 /QxHost /MT /fpp /traceback /heap-arrays:128"
set CMAKE_C_FLAGS_DEBUG="/Od /MTd /debug:full"
set CMAKE_C_FLAGS_PROFILE="/Od /MTd /debug:full"
set CMAKE_C_FLAGS_RELEASE="/O2 /QxHost /MT"

:: set the type of CMAKE OUTPUT to generate
set CMAKE_OUTPUT="NMake Makefiles"

for %%i in (%CMAKE_OUTPUT%) do (

echo "Running CMake for target %%i"
cmake ..\..\.. -G %%i ^
-DCMAKE_Fortran_COMPILER=%Fortran_COMPILER_NAME%              ^
-DCMAKE_C_COMPILER=%CMAKE_C_COMPILER%                         ^
-DCMAKE_BUILD_TYPE=%BUILD_TYPE%                               ^
-DCMAKE_INSTALL_PREFIX:PATH=%INSTALL_PREFIX%                  ^
-DCMAKE_Fortran_FLAGS_DEBUG=%CMAKE_Fortran_FLAGS_DEBUG%       ^
-DCMAKE_Fortran_FLAGS_PROFILE=%CMAKE_Fortran_FLAGS_PROFILE%   ^
-DCMAKE_Fortran_FLAGS_RELEASE=%CMAKE_Fortran_FLAGS_RELEASE%   ^
-DCMAKE_EXE_LINKER_FLAGS=%CMAKE_EXE_LINKER_FLAGS%             ^
-DCMAKE_C_FLAGS_DEBUG=%CMAKE_C_FLAGS_DEBUG%                   ^
-DCMAKE_C_FLAGS_PROFILE=%CMAKE_C_FLAGS_PROFILE%               ^
-DCMAKE_C_FLAGS_RELEASE=%CMAKE_C_FLAGS_RELEASE%

)
