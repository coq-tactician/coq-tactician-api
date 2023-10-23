"""For each lemma in some test packages, calculate the number of definitions it
depends on that are not in the training set"""

from pathlib import Path
import sys
from pytact.data_reader import data_reader, Original, definition_dependencies

test_packages = set([
    "coq-bits.1.1.0",
    "coq-qcert.2.2.0",
    "coq-ceres.0.4.0",
    "coq-corn.8.16.0",
    "coq-bytestring.0.9.0",
    "coq-hammer.1.3.2+8.11",
    "coq-gaia-stern.1.15",
    "coq-mathcomp-apery.1.0.1",
    "coq-tlc.20200328",
    "coq-iris-heap-lang.3.4.0",
    "coq-printf.2.0.0",
    "coq-smtcoq.2.0+8.11",
    "coq-topology.10.0.1"
    "coq-haskell.1.0.0",
    "coq-bbv.1.3",
    "coq-poltac.0.8.11",
    "coq-mathcomp-odd-order.1.14.0",
    "coq-hott.8.11"
])

def main():
    dataset_path = Path(sys.argv[1]).resolve()
    with data_reader(dataset_path) as data:

        graphid_in_test = [d.filename.parts[0] in test_packages for d in sorted(data.values(), key=lambda d: d.graph)]
        trans_deps = dict()

        def calc_trans_deps(d):
            if dist := trans_deps.get(d, None):
                return dist
            if not graphid_in_test[d.node.graph]:
                trans_deps[d] = set()
                return set()
            dist = set()
            dist.update(d.cluster)
            direct_cluster_deps = { dep for c in d.cluster for dep in definition_dependencies(c) } - set(d.cluster)
            for dep in direct_cluster_deps:
                dist.update(calc_trans_deps(dep))
            trans_deps[d] = dist
            return dist

        for f in data.values():
            if f.filename.parts[0] not in test_packages:
                continue
            for d in f.definitions():
                if not isinstance(d.status, Original):
                    continue
                if proof := d.proof:
                    deps = calc_trans_deps(d) - set(d.cluster)
                    print(f"{f.filename.parts[0]}\t{d.name}\t{len(deps)}")

if __name__ == "__main__":
    exit(main())
