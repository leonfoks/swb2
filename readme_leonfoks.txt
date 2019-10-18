module load gcc/6.1
module load cmake/3.14.2
module load hdf5-serial/1.10.1-gcc6.1.0
module load netcdf-serial/4.4.1.1-hdf51.10.1-gcc6.1.0

export ZLIB_ROOT=/cxfs/projects/spack/opt/spack/linux-scientific6-x86_64/gcc-6.1.0/zlib-1.2.11-ybmzdn63hza4u56ny7uqbngjzj57gajo/lib/

CMAKE -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_C_COMPILER=gcc ..
