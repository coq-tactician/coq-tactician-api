import pkg_resources

def graph_api_capnp():
    return pkg_resources.resource_filename('pytact','graph_api.capnp')

def test_filename():
    return pkg_resources.resource_filename('pytact','tests/ReinforceTest.v')

