#!/bin/bash

ARCHIVENAME="v1.0.tar.gz"

echo 'Fetching ndyacclex release from github'
wget "https://github.com/dmand/ndyacclex/archive/$ARCHIVENAME"
echo 'Unpacking ndyacclex'
mkdir /tmp/ndyacclex
tar -xf "$ARCHIVENAME" -C /tmp/
rm "$ARCHIVENAME"
pushd /tmp/ndyacclex-1.0
pushd src/lex
lazbuild ndlex.lpr
popd
pushd src/yacc
lazbuild ndyacc.lpr
popd
popd
cp /tmp/ndlexyacc/src/{lex/ndlex,yacc/ndyacc} bin/
