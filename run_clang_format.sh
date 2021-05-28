#!/bin/bash

git ls-files -- '*.cc' '*.hh' | xargs clang-format -i -style=file
git diff --exit-code --color
