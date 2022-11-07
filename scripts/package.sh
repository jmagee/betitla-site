#!/bin/sh

deploy_place=$(pwd)/deploy-me

stack build --local-bin-path=$deploy_place --copy-bins
cp -rv config $deploy_place
cp -rv static $deploy_place

echo "$deploy_place ready"
