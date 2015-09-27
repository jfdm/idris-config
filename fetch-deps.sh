#!/bin/bash

echo "Creating Dir"
mkdir -p dependancies/
cd dependancies/

echo "Fetching Deps for Config"

echo "Fetching lightyear "
git clone git@github.com:ziman/lightyear.git lightyear
cd lightyear/
make install
cd ../

echo "Fetching testing"
git clone git@github.com:jfdm/idris-testing.git testing
cd testing/
make install
cd ../

echo "Fetching Containers"
git clone git@github.com:jfdm/idris-containers.git containers
cd containers/
make install
cd ../

cd ../
