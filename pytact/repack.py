import sys
import os
from pathlib import Path
import capnp
import pytact.graph_api_capnp as api

max_node = {}
node_total = 0

rootdir = sys.argv[1]
os.chdir(rootdir)
rootdir = Path(os.getcwd())
file_list = [f for f in rootdir.glob('**/*.bin') if f.is_file()]

for f in file_list:
    print(f)
    fin = open(f)
    g = api.Graph.read(fin, traversal_limit_in_words=2**64-1)
    fout = open(f.with_suffix('.repacked'), 'w+b')
    g.as_builder().write(fout)
