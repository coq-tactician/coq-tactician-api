
from pathlib import Path
from pytact.data_reader import data_reader
import polars as pl

dataset_path = Path("/home/jgallagher/LocalData/tactician/v15-opam-coq8.11-partial/dataset").resolve()


with data_reader(dataset_path) as data:
    # data is everything.
    # data.items() iterates over all the files in the dataset dictionary.
    for idx, (relative_path,dataset) in enumerate(data.items()):
        data_lowlevel = dataset.lowlevel
        # type(www) is the lowevel pytact.graph_api_capnpn_cython.Dataset_Reader class
        # it has the fields:
        # - data_version: the "version of the data in this dataset" 
        # - definitions: the numbers (indices) of all the definitions in this file
        # - dependencies: the list of the Pathlike strings that contain the dependencies of this file, qua capnp serializations.
        # - dynamic: it looks like it's a dump of the graph the representative ,and other bits of information as a struct.
        # - graph: a graph reader -- gonna have to break this down elsewhere...
        # - has_data_version: true when the data_version field is set (like the other has_ names, this is coming from the capnp description allowing some fields to be optional)
        # - has_dependencies: true when the dependencies field is set
        # - has_graph: true when the graph field is set
        # - has_module_name: true when the module_mame field is set
        # - module_name: the module name that the file resides in
        # - representative: int -- the official docs say that this is the entrypoint of the global context of of definitions available when this file is required by another.  If no 'super'-global definitions, then this is set to len(graph.nodes).  But it's not perfectly clear what that means.
        print(dir(data_lowlevel))
        print(type(data_lowlevel))

        all_depIndices = [edge.target.dep_index for edge in data_lowlevel.graph.edges]
        all_depIndices_df = pl.DataFrame(all_depIndices)

        all_nodeIndices = [edge.target.node_index for edge in data_lowlevel.graph.edges]
        all_nodeIndices_df = pl.DataFrame(all_nodeIndices)
        
        breakpoint()


 




# depIndex and nodeIndex
# depIndex: 
#   - Indicates to which graph a node belongs. How this should be resolved is not specified here.  However, a value of zero always points to the 'current' graph.
#   - The graph contained in a file may reference nodes from the graph of other files. This field maps
#       + a `DepIndex` into the a file that contains that particular node.
#       + The first file in this list is always the current file. It is guaranteed that no cycles exist in
#       + the dependency relation between files induced by this field (except for the self-reference of the file).


# nodeIndex:
#   - The index into `Graph.nodes` where this (the current) node can be found.

# The ordering of dataset.lowlevel.dependencies is such that dataset.lowlevel.dependencies[0] is the pointer to the file implementing the capnp descriptoin of the current file's module_name: dataset.lowlevel.module_name


