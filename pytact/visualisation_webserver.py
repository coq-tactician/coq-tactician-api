from pathlib import Path
import argparse

from pytact.data_reader import data_reader
from pytact.graph_visualize_browse import GraphVisualisationBrowser, UrlMaker

from sanic import Sanic
from sanic.response import html

app = Sanic("graph-visualizer")

@app.get('/<path:path>/definition/<defid:int>/proof/step/<stepi:int>/outcome/<outcomei:int>')
async def outcome(request, path: str, defid: str, stepi: str, outcomei: str):
    return html(app.ctx.gv.outcome(Path(path).with_suffix(".bin"), int(defid), int(stepi), int(outcomei)))

@app.get('/<path:path>/definition/<defid:int>/proof')
async def proof(request, path: str, defid: str):
    return html(app.ctx.gv.proof(Path(path).with_suffix(".bin"), int(defid)))

@app.get('/<path:path>/definition/<defid:int>')
async def definition(request, path: str, defid: str):
    return html(app.ctx.gv.definition(Path(path).with_suffix(".bin"), int(defid)))

@app.get('/<path:path>/context')
async def global_context(request, path: str):
    return html(app.ctx.gv.global_context(Path(path).with_suffix(".bin")))

@app.get('/<path:path>')
async def folder(request, path: str):
    return html(app.ctx.gv.folder(Path(path)))

@app.get('/')
async def root_folder(request):
    return html(app.ctx.gv.folder(Path()))

class SanicUrlMaker(UrlMaker):
    def definition(self, fname: Path, defid: int) -> str:
        return app.url_for('definition', path=fname.with_suffix(''), defid=defid)
    def proof(self, fname: Path, defid: int) -> str:
        return app.url_for('proof', path=fname.with_suffix(''), defid=defid)
    def outcome(self, fname: Path, defid: int, stepi: int, outcomei: int) -> str:
        return app.url_for('outcome', path=fname.with_suffix(''),
                           defid=defid, stepi=stepi, outcomei=outcomei)
    def global_context(self, fname: Path) -> str:
        return app.url_for('global_context', path=fname.with_suffix(''))
    def folder(self, path: Path) -> str:
        return app.url_for('folder', path=path)
    def root_folder(self) -> str:
        return app.url_for('root_folder')

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
        app.ctx.gv = GraphVisualisationBrowser(data, SanicUrlMaker())
        app.run(host=args.hostname, port=args.port, dev=args.dev)

if __name__ == '__main__':
    main()
