# Base system setup

## Macports users

  sudo port install gmp clang-devel graphviz capnproto libev xxhash xxhashlib
  sudo port select --set python python310

## Mac + brew users

  Leave steps here if you get this working with brew

## Windows/WSL users

  sudo apt-get --yes install graphviz capnproto libcapnp-dev pkg-config libev-dev libxxhash-dev
  sudo add-apt-repository pap:deadsnakes/ppa
  sudo apt update
  sudo apt install python3.11
  sudo apt install python3.11-venv
  python3 -m venv .venv 
  source .venv/bin/activate
  python3 -m pip install --upgrade pip
  python3 -m pip install pytactician

# Python specific system setup

  python3 -m venv .venv
  source .venv/bin/activate
  python3 -m pip install --upgrade pip
  python3 -m pip install -r experiments-jdg/requirements.txt