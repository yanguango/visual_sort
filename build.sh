#!/bin/bash
if [ $# -eq 0 ]
  then
    echo "No file input"
else
  ocamlfind ocamlc -package js_of_ocaml -syntax camlp4o -package js_of_ocaml.syntax -linkpkg -o $1.byte yg_sort.ml $1.ml
  js_of_ocaml $1.byte -o sites/$1.js
fi
