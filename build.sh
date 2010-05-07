#!/bin/sh

## Make all the js files for the sandbox files.

olddir=`pwd`;
for file in sandbox/*/*/*_merged_ss.zo
do
    cd `dirname ${file}`
    mzscheme ${olddir}/src/mzjs.ss `basename ${file}`
    cd ${olddir}
done

cd ${olddir}