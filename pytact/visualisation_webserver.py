# Python 3 server example
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
import time
import sys
import os

from pytact.graph_visualize_browse import GraphVisualisationBrowser

#hostNameExternal = "64.71.146.87"
#hostName = "10.64.66.7"
hostName = "localhost"
hostNameExternal = hostName
serverPort = 8080
gv = GraphVisualisationBrowser(
    sys.argv[1],
    "http://{}:{}/".format(hostNameExternal, serverPort),
)

class VisualisationServer(BaseHTTPRequestHandler):
    def do_GET(self):

        print("path:", self.path)
        dirname, basename = os.path.split(self.path)
        dirname = dirname.removeprefix('/')
        if basename == "file_deps.svg":
            self.send_svg(gv.file_deps(dirname.split('/')))
            return
        fname = dirname+".bin"
        print("fname:", fname)

        if basename == "index.svg":
            self.send_svg(gv.global_context(fname))
            return
        elif basename == "dependencies.svg":
            self.send_svg(gv.definition_dependencies(fname))
            return
        if basename.startswith("definition-"):
            basename = basename.removeprefix("definition-").removesuffix(".svg")
            if basename.isdigit():
                defid = int(basename)
                self.send_svg(gv.definition(fname, defid))
                return
            else:
                defid, proof_label, *proof_args = basename.split('-')
                assert proof_label == "proof"
                defid = int(defid)
                if not proof_args:
                    self.send_svg(gv.proof(fname, defid))
                    return
                step_label, step_i, outcome_label, outcome_i = proof_args
                assert step_label == "step"
                assert outcome_label == "outcome"
                step_i = int(step_i)
                outcome_i = int(outcome_i)
                self.send_svg(gv.outcome(fname, defid, step_i, outcome_i))
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
    webServer = HTTPServer((hostName, serverPort), VisualisationServer)
    print(f"Server started {gv.root_file_url()}")

    try:
        webServer.serve_forever()
    except KeyboardInterrupt:
        pass

    webServer.server_close()
    print("Server stopped.")

if __name__ == "__main__":
    main()
