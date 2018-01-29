#!/bin/bash -xue

export TEST_HOME='/home/jenkins/arakoon/TESTS'


case "${1-undefined}" in
    unit)
        make build
        ./arakoon.native --run-all-tests-xml testresults.xml
        sudo cp testresults.xml /home/arakoon
        ;;
    kwik)
        ./jenkins/kwik.sh
        ;;
    b)
        ./jenkins/B.sh
        ;;
    c)
        ./jenkins/C.sh
        ;;
    d)
        ./jenkins/D.sh
        ;;
    nose)
        shift
        ./jenkins/nose.sh $@
        ;;
    package_deb)
        ./jenkins/package_deb.sh
        ;;
    package_rpm)
        ./jenkins/package_rpm.sh
        ;;
    *)
        echo "invalid test suite specified"
esac
