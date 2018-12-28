#!/bin/bash

clj -A:rebl -m nrepl.cmdline --port 5182 --middleware '[nrepl-rebl.core/wrap-rebl]'
