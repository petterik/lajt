#!/bin/bash

script_dir=$(dirname $0)

"$script_dir/cljs.sh" -re node --main lime.tests
