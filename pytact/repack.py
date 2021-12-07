import sys
import os
from pathlib import Path
import capnp
import graph_api_capnp

max_node = {}
node_total = 0

rootdir = sys.argv[1]
os.chdir(rootdir)
rootdir = Path(os.getcwd())
file_list = [f for f in rootdir.glob('**/*.bin') if f.is_file()]

for f in file_list:
    print(f)
    fin = open(f)
    g = graph_api_capnp.Graph.read_packed(fin, traversal_limit_in_words=2**64-1)
    fout = open(f.with_suffix('.repacked'), 'w+b')
    g.as_builder().write(fout)
