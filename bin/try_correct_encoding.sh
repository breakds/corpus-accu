#!/bin/bash

# $1 = input file
# $2 = output file

rm -rf "$2"
/usr/bin/iconv -f cp936 -t utf-8 "$1" > "$2"

if [[ $? != "0" ]]; then
    cp -f $1 $2
fi


    

