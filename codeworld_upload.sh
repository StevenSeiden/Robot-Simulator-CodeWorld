#!/bin/sh

# Uploads one or more source files to a code.world installation

#set -e

mode=""
url=""

if [ "x$1" = "x-s" ]; then
    url="https://blackeyepeas.phys.lsu.edu/cw/"
    mode=codeworld
    shift
fi

if [ "x$1" = "x-s2" ]; then
    url="http://blackeyepeas.phys.lsu.edu:8081/"
    mode=codeworld
    shift
fi

if [ -z "$url" -a "x$1" != "x" ]; then
    url="$1"
    shift
fi

if [ -z "$mode" -a "x$1" != "x" ]; then
    mode="$1"
    shift
fi

if [ -z "$url" -o -z "$mode" ]; then
    echo "Usage: $0 http://code.world.url/ [haskell|codeworld] [source.hs ...]"
    echo " $0 -s [source.txt ...]"
    exit 1
fi

for file in "$@"; do
    echo "Uploading $file ..."
    echo "curl -S -s -F \"mode=$mode\" -F \"source=<$file\" ${url}compile"
    json="$(curl -S -s -F "mode=$mode" -F "source=<$file" ${url}compile)"
    dhash="$(echo "$json" | jq -r .dhash)"
    hash="$(echo "$json" | jq -r .hash)"
    if [ "$mode" = "codeworld" ]; then
        curl="$url#$hash"
    else
        curl="$url$mode#$hash"
    fi
    durl="${url}run.html?mode=$mode&dhash=$dhash"
    echo "Source at $curl"
    echo "Runnable at $durl"
    firefox $durl &
    echo
done
