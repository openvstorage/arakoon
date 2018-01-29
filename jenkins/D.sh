#!/bin/bash -xue

./jenkins/common.sh

python test_it.py -v -s --with-xunit --xunit-file=testresults.xml server/shaky
sudo cp testresults.xml /home/arakoon
