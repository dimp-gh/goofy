#!/bin/bash

ARCHIVENAME="v1.0.tar.gz"

echo "$0: Fetching ndyacclex release from github"
wget "https://github.com/dmand/ndyacclex/archive/$ARCHIVENAME"
echo "$0: Unpacking ndyacclex"
tar -xf "$ARCHIVENAME" -C /tmp/
rm "$ARCHIVENAME"
pushd /tmp/ndyacclex-1.0
pushd src/lex
echo "$0: Building lex"
lazbuild ndlex.lpr
popd
pushd src/yacc
echo "$0: Building yacc"
lazbuild ndyacc.lpr
popd
popd
echo "$0: Copying binaries"
cp /tmp/ndyacclex-1.0/src/{lex/ndlex,yacc/ndyacc} bin/
echo "$0: Success"
