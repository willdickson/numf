project('my_test', 'fortran')

fortran_dialect = [ '-ffree-form','-std=f2008','-fimplicit-none']
fortran_bounds = [ '-fbounds-check']

add_global_arguments(fortran_dialect, language : 'fortran')
add_global_arguments(fortran_bounds, language : 'fortran')
add_global_arguments(['-cpp'], language : 'fortran')

lapack = dependency('lapack')

fmain_sources = [ 
    'src/main.f90', 
    'src/poly1d_mod.F90',
    'src/lstsq_mod.F90', 
    'src/array_create_mod.F90',
    'src/rtn_info_mod.f90',
    'src/flab_kinds_mod.f90',
    ]
executable('fmain', fmain_sources, dependencies : [lapack])


