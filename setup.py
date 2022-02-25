"""
packaging the python tests to coq-tactician-reinforce
"""

import setuptools

CLASSIFIERS = """\
Development Status :: 1 - Planning
Intended Audience :: Science/Research
Intended Audience :: Developers
License :: OSI Approved :: MIT License
Programming Language :: C
Programming Language :: Python
Programming Language :: Python :: 3
Programming Language :: Python :: 3.8
Programming Language :: Python :: 3 :: Only
Programming Language :: Python :: Implementation :: CPython
Topic :: Scientific/Engineering :: Artificial Intelligence
Operating System :: POSIX :: Linux
"""


setuptools.setup(
    name='pytact',
    version='8.11.0.1.dev0',
    author='Lasse Blaauwbroek <lasse@blaauwbroek.eu>',
    packages=['pytact'],
    license='MIT License',
    long_description='python interface to coq-tactician-reinforce',
    long_description_content_type='text/markdown',
    url='https://github.com/coq-tactician/coq-tactician-reinforce',
    python_requires='>=3.7',
    include_package_data=True,
    package_data = {'pytact': ['graph_api.capnp',
                               'tests/ReinforceTest.v']},
    install_requires=['pycapnp', 'graphviz', 'ptpython'],
    entry_points={'console_scripts': ['pytact-test=pytact.fake_reinforcement_client:run_main',
                                      'pytact-server=pytact.fake_python_server:run_main',
                                      'pytact-check=pytact.graph_sanity_check:main']},
    project_urls={
        'Source': 'https://github.com/coq-tactician/coq-tactician-reinforce'
    },
    platforms = ["Linux"],
    classifiers=[_f for _f in CLASSIFIERS.split('\n') if _f],
    )
