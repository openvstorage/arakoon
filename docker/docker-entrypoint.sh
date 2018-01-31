#!/bin/bash -l
# this script is executed at each startup of the container
set -e
cd $HOME

# make a local copy of sources
sudo tar cf - -C /home arakoon | tar xf -

cd arakoon

# finally execute the command the user requested
case ${1-sh} in
  sh)
	shift
	exec sh "$@"
	;;
  clean)
	make clean
	;;
  build)
	make build
	;;
  *)    # everything else is considered as a jenkins suite
	docker/suites.sh "$@"
	;;
esac
