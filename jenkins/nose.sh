#!/bin/bash -xue

./jenkins/common.sh
echo "nose.sh:PARAMETERS=$@"
python test_it.py -v -s --with-xunit --xunit-file=testresults.xml $@
sudo cp testresults.xml /home/arakoon
