#!/bin/bash

Cur=$(pwd)
List=( priv ebin )
if [ ! -f amazon_private_modules.list ];
then 
  if [ -f amazon_private_modules.list.example ]; 
  then
    echo "Create modules list from example"
    cp amazon_private_modules.list.example amazon_private_modules.list;
    else 
      echo "Config not found -- FAILL"
      exit
  fi
fi

Dirs=$(cat ./amazon_private_modules.list)

for i in $Dirs ;
do
  Name=(`echo $i | tr ':' '\n'`);
  Dest=$Cur/erlyvideo/lib/${Name[0]};
  if [ ! -d $Dest ]; 
  then
    echo "Create directory "$Dest
    mkdir $Dest;
  fi;
  cd ${Name[1]};
  for src in ${List[@]} ;
  do
    if [ ! $(cp -fr $src $Dest) ] ;  # when cp success it return 0
    then
      echo "Copy "$src" to "$Dest" is complited -- OK" 
    else
     echo "Copy "$src" to "$Dest" is complited -- OK" 
    fi
  done
  cd $Cur; 
done
