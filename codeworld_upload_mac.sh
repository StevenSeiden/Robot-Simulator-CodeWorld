#!/bin/bash

# Uploads one or more source files to a code.world installation

#set -e

mode=""
url=""

if [ "x$1" = "x-s" ]; then
    url="https://blackeyepeas.phys.lsu.edu/cw/"
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
    #echo "curl -S -s -F \"mode=$mode\" -F \"source=<$file\" ${url}compile"
    json="$(curl -S -s -F "mode=$mode" -F "source=<$file" ${url}compile)"
    dhash="$(echo "$json" | jq -r .dhash)"
    hash="$(echo "$json" | jq -r .hash)"
    errormsg="$(curl -S -s -F "mode=$mode" -F "hash=$hash" ${url}runMsg)"
    if [ "$mode" = "codeworld" ]; then
            curl="$url#$hash"
        else
            curl="$url$mode#$hash"
        fi
    if [ -z "$errormsg" ] || [[ $errormsg == *"warning"* ]]; then
        durl="${url}run.html?mode=$mode&dhash=$dhash"
        echo -e "\e[92mSuccessfully uploaded files!\e[0m"
        echo "Source at: $curl"
        echo "Runnable at: $durl"
        if [ ! -z "$errormsg" ]; then 
            echo -e "\e[33mCompiled with warnings:"
            echo ${errormsg}
            echo -e "\e[0m"
        fi
        if [ ! -z "${DISPLAY}" ]; then
            open $durl &
        fi
        echo
    else
        echo
        echo -e "\e[91m${file} Compilation failed:"
        echo ${errormsg}
        echo -e "\e[0m"
        echo "Source at: $curl"
        open $curl
        echo
    fi
done