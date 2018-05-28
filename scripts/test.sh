#!/bin/bash

script_dir=$(dirname $0)

echo "########"
echo "RUNNING clj tests" 
echo "########"

clj -A:test --main lajter.tests

echo "########"
echo "RUNNING cljs tests" 
echo "########"

clj -A:test -m cljs.main -re node --main lajter.tests
