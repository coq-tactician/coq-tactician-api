"""
Python support for Tactician data
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
Programming Language :: Python :: 3 :: Only
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
    python_requires='>=3.10',
    include_package_data=True,
    package_data = {'pytact': ['graph_api.capnp',
                               'tests/TestReinforceStdin.v',
                               'tests/TestReinforceTcp.v',
                               'tests/prop4.txt']},
    install_requires=[
        'pycapnp',
        'pyrsistent'
        'graphviz',
        'sanic[ext]==22.6.2',
        'Jinja2'],
    entry_points={'console_scripts': [
                                      'pytact-server=pytact.fake_python_server:main',
                                      'pytact-check=pytact.graph_sanity_check:main',
                                      'pytact-visualize=pytact.visualisation_webserver:main',
                                      'pytact-prover=pytact.prover:main'
                                      ]
                  },
    project_urls={
        'Source': 'https://github.com/coq-tactician/coq-tactician-reinforce'
    },
    platforms = ["Linux"],
    classifiers=[_f for _f in CLASSIFIERS.split('\n') if _f],
    )
