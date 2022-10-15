# Python 3 server example
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
import time
import sys
import os
import argparse

from pytact.data_reader import data_reader
from pytact.graph_visualize_browse import GraphVisualisationBrowser

class VisualisationServer(BaseHTTPRequestHandler):
    def __init__(self, gv, *args):
        self.gv = gv
        BaseHTTPRequestHandler.__init__(self, *args)

    def do_GET(self):

        print("path:", self.path)
        dirname, basename = os.path.split(self.path)
        dirname = dirname.removeprefix('/')
        if basename == "file_deps.svg":
            self.send_svg(self.gv.file_deps(Path(dirname)))
            return
        fname = dirname+".bin"
        print("fname:", fname)

        if basename == "index.svg":
            self.send_svg(self.gv.global_context(Path(fname)))
            return
        if basename.startswith("definition-"):
            basename = basename.removeprefix("definition-").removesuffix(".svg")
            if basename.isdigit():
                defid = int(basename)
                self.send_svg(self.gv.definition(Path(fname), defid))
                return
            else:
                defid, proof_label, *proof_args = basename.split('-')
                assert proof_label == "proof"
                defid = int(defid)
                if not proof_args:
                    self.send_svg(self.gv.proof(Path(fname), defid))
                    return
                step_label, step_i, outcome_label, outcome_i = proof_args
                assert step_label == "step"
                assert outcome_label == "outcome"
                step_i = int(step_i)
                outcome_i = int(outcome_i)
                self.send_svg(self.gv.outcome(Path(fname), defid, step_i, outcome_i))
                return

        self.send_err()

    def send_svg(self, svg):
        self.send_response(200)
        self.send_header("Content-type", "image/svg+xml")
        self.end_headers()
        self.wfile.write(svg)

    def send_err(self):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        self.wfile.write(bytes("<html><head><title>404</title></head>", "utf-8"))
        self.wfile.write(bytes("<p>Request: %s</p>" % self.path, "utf-8"))
        self.wfile.write(bytes("<body>", "utf-8"))
        self.wfile.write(bytes("<p>Unexpected path</p>", "utf-8"))
        self.wfile.write(bytes("</body></html>", "utf-8"))

def main():

    parser = argparse.ArgumentParser(
        description = 'Dataset visualization webserver',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('dir',
                        type=str,
                        help='the directory of the dataset')
    parser.add_argument('--port',
                        type=int,
                        default=8080,
                        help='the port where the webserver should listen')
    parser.add_argument('--hostname',
                       type=str,
                       default='localhost',
                       help='the ip or domain of the hosting machine')

    args = parser.parse_args()

    dataset_path = Path(args.dir).resolve()
    with data_reader(dataset_path) as data:
        gv = GraphVisualisationBrowser(data, "http://{}:{}/".format(args.hostname, args.port))
        def handler(*args):
            return VisualisationServer(gv, *args)
        webServer = HTTPServer(('0.0.0.0', args.port), handler)
        print(f"Server started {gv.root_file_url()}")

        try:
            webServer.serve_forever()
        except KeyboardInterrupt:
            pass
        finally:
            webServer.server_close()
            print("Server stopped.")

if __name__ == "__main__":
    main()
