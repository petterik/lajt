#!/bin/bash

clj -A:dev:test:cljs-repl:fikes/watch-fn-fix -m lime.main.continous-tests
