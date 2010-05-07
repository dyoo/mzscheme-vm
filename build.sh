#!/bin/bash

## Make all the js files for the tests files.

batchcompiler="../batch/batch.ss"

basedir=`pwd`;

build_batch() {
## rebuild all the batch-compiled files
    cd ${basedir}
    for file in tests/*/*.ss
    do
	cd `dirname ${file}`
	mzscheme ${basedir}/${batchcompiler} `basename ${file}`
	cd ${basedir}
    done
    cd ${basedir}
}


build_mzjs() {
## run mzjs over all the files
    cd ${basedir}
    for file in tests/*/*/*_merged_ss.zo
    do
	cd `dirname ${file}`
	echo "Making `basename ${file}`"
	mzscheme ${basedir}/src/mzjs.ss `basename ${file}`
	cp *.js ..
	cd ${basedir}
    done
    cd ${basedir}
}


test_output() {
    cd ${basedir}
    for file in tests/*/*.js
    do
	cd ${basedir}
	cd `dirname ${file}`
	echo "Testing ${file}"
 	node `basename ${file}` > observed.txt
 	if [ -f expected.txt ]; then
  	    diff expected.txt observed.txt
  	else
  	    echo "No expected.txt to compare against"
  	    cat observed.txt
  	fi
    done
    cd ${basedir}
}



if [ "$1" == "mzjs" ]; then
    build_mzjs
elif [ "$1" == "batch" ]; then
    build_batch
elif [ "$1" == "test" ]; then
    test_output
else
    build_mzjs
fi


#build_mzjs
#build_batch