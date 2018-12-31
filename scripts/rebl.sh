#!/bin/bash

clj -A:dev:test:rebl -m nrepl.cmdline --port 5182 --middleware '[nrepl-rebl.core/wrap-rebl]'
