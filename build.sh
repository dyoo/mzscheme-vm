#!/bin/bash

## Make all the js files for the sandbox files.

batchcompiler="../batch/batch.ss"

olddir=`pwd`;

build_batch() {
## rebuild all the batch-compiled files
    cd ${olddir}
    for file in sandbox/*/*.ss
    do
	cd `dirname ${file}`
	mzscheme ${olddir}/${batchcompiler} `basename ${file}`
	cd ${olddir}
    done
    cd ${olddir}
}


build_mzjs() {
## run mzjs over all the files
    cd ${olddir}
    for file in sandbox/*/*/*_merged_ss.zo
    do
	cd `dirname ${file}`
	echo "Making `basename ${file}`"
	mzscheme ${olddir}/src/mzjs.ss `basename ${file}`
	cp *.js ..
	cd ${olddir}
    done
    cd ${olddir}
}


test_output() {
    echo "Not done yet"
}



if [ "$1" == "mzjs" ]; then
    build_mzjs
elif [ "$1" == "batch" ]; then
    build_batch
else
    build_mzjs
fi


#build_mzjs
#build_batch