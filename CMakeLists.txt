cmake_minimum_required(VERSION 3.5)
project(final_v1_0)

enable_language(Fortran)
add_executable(run.exe
        source/dfs.f90
        source/dir_init.f90
        source/func.f90
        source/main.f90
        source/tree.f90
        source/wood.f90
        source/wood_pot.f90)