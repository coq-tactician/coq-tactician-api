from dataclasses import asdict
from pathlib import Path
import argparse
from typing import Any

from pytact.data_reader import data_reader
from pytact.graph_visualize_browse import GraphVisualizationData, GraphVisualizator, UrlMaker, Settings

from sanic import Sanic
from sanic_ext import validate

def post_process_svg(svg):
    return svg.decode("utf-8").split('\n', 3)[-1]

app = Sanic("graph-visualizer")

class SanicUrlMaker(UrlMaker):

    def __init__(self, settings: Settings):
        self.query = {k: v for k, v in asdict(settings).items() if type(v) != bool or v}

    def definition(self, fname: Path, defid: int) -> str:
        return app.url_for('definition', path=fname.with_suffix(''), defid=defid, **self.query)

    def proof(self, fname: Path, defid: int) -> str:
        return app.url_for('proof', path=fname.with_suffix(''), defid=defid, **self.query)

    def outcome(self, fname: Path, defid: int, stepi: int, outcomei: int) -> str:
        return app.url_for('outcome', path=fname.with_suffix(''),
                           defid=defid, stepi=stepi, outcomei=outcomei, **self.query)

    def global_context(self, fname: Path) -> str:
        return app.url_for('global_context', path=fname.with_suffix(''), **self.query)

    def folder(self, path: Path) -> str:
        return app.url_for('folder', path=path, **self.query)

    def root_folder(self) -> str:
        return app.url_for('root_folder', **self.query)

@app.get('/<path:path>/definition/<defid:int>/proof/step/<stepi:int>/outcome/<outcomei:int>')
@validate(query=Settings)
@app.ext.template("visualizer.html")
async def outcome(request, path: str, defid: str, stepi: str, outcomei: str, query: Settings):
    gv = GraphVisualizator(app.ctx.gvd, SanicUrlMaker(query), query)
    return {"svg": post_process_svg(gv.outcome(Path(path).with_suffix(".bin"), int(defid), int(stepi), int(outcomei))),
            "settings": query}

@app.get('/<path:path>/definition/<defid:int>/proof')
@validate(query=Settings)
@app.ext.template("visualizer.html")
async def proof(request, path: str, defid: str, query: Settings):
    gv = GraphVisualizator(app.ctx.gvd, SanicUrlMaker(query), query)
    return {"svg": post_process_svg(gv.proof(Path(path).with_suffix(".bin"), int(defid))),
            "settings": query}

@app.get('/<path:path>/definition/<defid:int>')
@validate(query=Settings)
@app.ext.template("visualizer.html")
async def definition(request, path: str, defid: str, query: Settings):
    gv = GraphVisualizator(app.ctx.gvd, SanicUrlMaker(query), query)
    return {"svg": post_process_svg(gv.definition(Path(path).with_suffix(".bin"), int(defid))),
            "settings": query}

@app.get('/<path:path>/context')
@validate(query=Settings)
@app.ext.template("visualizer.html")
async def global_context(request, path: str, query: Settings):
    gv = GraphVisualizator(app.ctx.gvd, SanicUrlMaker(query), query)
    return {"svg": post_process_svg(gv.global_context(Path(path).with_suffix(".bin"))),
            "settings": query}

@app.get('/<path:path>')
@validate(query=Settings)
@app.ext.template("visualizer.html")
async def folder(request, path: str, query: Settings):
    gv = GraphVisualizator(app.ctx.gvd, SanicUrlMaker(query), query)
    return {"svg": post_process_svg(gv.folder(Path(path))),
            "settings": query}

@app.get('/')
@validate(query=Settings)
@app.ext.template("visualizer.html")
async def root_folder(request, query: Settings):
    gv = GraphVisualizator(app.ctx.gvd, SanicUrlMaker(query), query)
    return {"svg": post_process_svg(gv.folder(Path())),
            "settings": query}

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
                       default='0.0.0.0',
                       help='the ip or domain of the hosting machine')
    parser.add_argument('--dev',
                        action='store_true',
                        help='run the server in development mode')

    args = parser.parse_args()

    dataset_path = Path(args.dir).resolve()
    with data_reader(dataset_path) as data:
        app.ctx.gvd = GraphVisualizationData(data)
        app.run(host=args.hostname, port=args.port, dev=args.dev)

if __name__ == '__main__':
    main()
