#!/bin/sh

bump="$1"

function bump_version {
    VER="$1"
    LEVEL="$2"
    div=
    i=0
    for SUB in $(echo $VER | tr "\." "\n")
    do
        if [[ $i == $LEVEL ]]
        then
            NEWVER=$NEWVER$div$(expr $SUB + 1)
            i=$(expr $i + 1)
            break
        fi
    
        NEWVER=$NEWVER$div$SUB
        div=.
        i=$(expr $i + 1)
    done
 
    if [[ $(expr $i - 1) != $LEVEL ]]
    then
        NEWVER=$NEWVER$div\1
    fi
    echo $NEWVER
}

level=-1

if [ "$bump" == "major" ]; then
    level=0
fi
if [ "$bump" == "minor" ]; then
    level=1
fi
if [ "$bump" == "patch" ]; then
    level=2
fi

vstring=$(git describe HEAD --tags | rev | sed 's/g-/./' | sed 's/-/+/' | rev)
if echo "$vstring" | grep -q '\+'; then
    pvstring=`echo $vstring | 
         sed 's/\(^[0-9\.]*\)[^+]*[+]*\([0-9][0-9]*\)\.\([0-9a-f]*\)$/\1|\2|\3/'`
    version=`echo $pvstring | cut -f1 -d"|"`
    dist=-`echo $pvstring | cut -f2 -d"|"`
    hash=`echo $pvstring | cut -f3 -d"|"`

    ## Erase trailing zeros from version
    while echo $version | grep -q '\.0$'; do
	version=`echo $version | sed 's/\.0$//'`
    done

    ## Decrease last number
    if echo $version | grep -q '\.'; then
	lastnum=`echo $version | sed 's/.*\.\([0-9]*\)$/\1/'`
	rest=`echo $version | sed 's/\(.*\.\)[0-9]*$/\1/'`
    else 
	lastnum=$version
	rest=""
    fi

    if [ $level -gt -1 ]; then
        echo $(bump_version $version $level)
    else
        ## Add .999 and distance
        echo ${rest}$((lastnum)).999${dist}
    fi

else 
    echo $vstring
fi