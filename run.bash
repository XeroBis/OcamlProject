#!/bin/bash

ocamlfind ocamlc main.ml -o build/main -linkpkg -package graphics
./build/main