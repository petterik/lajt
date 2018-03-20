#!/bin/bash

script_dir=$(dirname $0)

echo "########"
echo "RUNNING clj tests" 
echo "########"

$script_dir/run-clj-tests.sh

echo "########"
echo "RUNNING cljs tests" 
echo "########"

$script_dir/run-cljs-tests.sh
