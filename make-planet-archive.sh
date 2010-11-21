#!/bin/bash
MAJOR=1
MINOR=12
PROJNAME=js-vm


OLDDIR=`pwd`
mkdir -p tmp
rm -rf tmp/$PROJNAME
git archive --format=tar --prefix=js-vm/ HEAD | (cd tmp && tar xf -)

cd $OLDDIR/tmp/$PROJNAME/private
rm -rf sandbox


cd $OLDDIR/tmp

raco planet unlink dyoo $PROJNAME.plt $MAJOR $MINOR
raco planet link dyoo $PROJNAME.plt $MAJOR $MINOR $PROJNAME

raco planet create $PROJNAME

raco planet unlink dyoo $PROJNAME.plt $MAJOR $MINOR

cd $OLDDIR
cp tmp/$PROJNAME.plt .