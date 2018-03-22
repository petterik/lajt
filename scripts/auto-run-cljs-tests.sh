#!/bin/bash

clj -A:dev:test:cljs-repl:fikes/watch-fn-fix -m lajt.main.continous-tests
