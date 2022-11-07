#!/bin/bash
# Extract Term files from repo.

mkdir local_work
pushd local_work

git clone https://github.com/jmagee/betitla betitla
rsync -a betitla/Terms ../betitla-data/
rm -rf betitla
popd

rm -r local_work
