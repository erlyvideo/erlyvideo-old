#!/bin/bash
#===================================================================================
#        FILE:  make-tarball.sh
#       USAGE:  make-tarball.sh version
# DESCRIPTION:  Create proper tarball for RPM-package build
# PARAMETER 1:  version
#===================================================================================

usage="Usage: $0 version"
[[ "$#" -lt 1 ]] && { echo $usage; exit 1; }

version=$1
pwd=$(pwd)
name=${pwd##*/}-${version}
mkdir -p tarballs

# Don't add to tarball:
# - this script itself
# - spec file for RPM package
# - git index
# - other tarballs 
tar --exclude "$0" --exclude "*.spec" --exclude ".git*" --exclude "tarballs" --transform "s#./#${name}/#" -cvzf "tarballs/${name}.tar.gz" .
