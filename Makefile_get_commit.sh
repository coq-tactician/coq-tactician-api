#!/bin/sh

get_commit() {
commit_status=$(git submodule status coq-tactician)
if expr "${commit_status}" : ' .*' > /dev/null; then
    echo "${commit_status}" | cut -c2-41
elif expr "${commit_status}"  : '\+.*' > /dev/null; then
    echo 'Error: submodule coq-tactician is not in sync.' >&2
    echo 'Execute "git add coq-tactician" and this script again.' >&2
    exit 1
else
    echo 'Failure: git submodule status does not start by a space or +' >&2
    echo "${commit_status}" >&2
    exit 1
fi
}

get_commit


    

