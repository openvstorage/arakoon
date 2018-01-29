#!/bin/bash -xue

./jenkins/common.sh

python test_it.py -v --with-xunit --xunit-file=testresults.xml server/quick
sudo cp testresults.xml /home/arakoon
