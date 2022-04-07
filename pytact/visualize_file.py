from pathlib import Path
import sys
import os
import webbrowser
from pathlib import Path

import capnp
capnp.remove_import_hook()
import pytact.common
import pytact.graph_visualize as gv

graph_api_capnp = pytact.common.graph_api_capnp()
graph_api_capnp = capnp.load(graph_api_capnp)

def absolute_deps(root, deps):
    return [ os.path.join(root, dep) for dep in deps ]

def get_graph(dep):
    f = open(dep)
    g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
    # Needed to work around this annoying bug: https://github.com/capnproto/pycapnp/issues/82
    g.total_size
    return g.graph

def main():
    file = os.path.abspath(sys.argv[1])
    f = open(file)
    g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
    # Needed to work around this annoying bug: https://github.com/capnproto/pycapnp/issues/82
    g.total_size
    root = file.removesuffix(g.dependencies[0])
    print(root)
    dependencies = absolute_deps(root, g.dependencies)
    graphs = [g.graph] + [get_graph(dep) for dep in dependencies[1:]]

    context_file = gv.visualize_global_context(root, graphs, g.definitions, g.representative, dependencies)

    for definition in g.definitions:
        print(g.graph.nodes[definition].label.definition.name)
        gv.visualize_definition(root, graphs, definition, dependencies)

    for definition in g.definitions:
        print(g.graph.nodes[definition].label.definition.name)
        gv.visualize_definition(root, graphs, definition, dependencies)

    gv.visualize_definition_dependencies(root, graphs, g.definitions, dependencies, g.representative)

    print(context_file)
    #webbrowser.open_new_tab(context_file)

def generate_index():

    deps = {}

    rootdir = Path(os.path.expanduser(sys.argv[1]))
    for f in [f for f in rootdir.glob('**/*.bin') if f.is_file()]:
        with open(f) as f:
            g = graph_api_capnp.Dataset.read_packed(f, traversal_limit_in_words=2**64-1)
            deps[g.dependencies[0]] = list(g.dependencies)[1:]
            # Needed to work around this annoying bug: https://github.com/capnproto/pycapnp/issues/82
            g.total_size
    file = gv.visualize_file_deps(rootdir, deps)
    webbrowser.open_new_tab(file)
