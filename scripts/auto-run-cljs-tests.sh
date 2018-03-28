#!/bin/bash

clj -A:dev:test:cljs-repl:cljs/dev -m lajt.main.continous-tests
