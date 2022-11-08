from setuptools import setup, Extension
import subprocess
import os
from shutil import copyfile
import capnp

from Cython.Build import cythonize

def capnp2cython():

    api_file = "pytact/graph_api.capnp"
    cython_file = api_file.replace('.', '_')+'_cython.pyx'

    # Poor man's make
    target_time = min([(os.path.getmtime(f) if os.path.exists(f) else 0) for f in
                        [api_file+'.c++', api_file+'.cpp', api_file+'.h', cython_file]])
    source_time = os.path.getmtime(api_file)
    if target_time < source_time:
        print(subprocess.check_output(["capnpc", "-oc++", api_file]))
        # subprocess.check_output(["capnpc", "-o./pytact/generate_api.py", api_file])

        copyfile(api_file + '.c++', api_file + '.cpp')

    capnp_loc = os.path.abspath(os.path.join(list(capnp.__path__)[0], '../'))
    return cythonize([
        Extension(
            name = "pytact.graph_api_capnp_cython",
            sources = [cython_file],
            extra_compile_args=["-O3"],
            include_dirs=[capnp_loc]
        ),
        Extension(
            name = "pytact.data_reader",
            sources = ['pytact/data_reader.pyx'],
            extra_compile_args=["-O3"],
            include_dirs=[capnp_loc]
        )])

setup(
    ext_modules = capnp2cython()
)
