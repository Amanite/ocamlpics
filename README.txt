ocamlpics
=========

genetic algorithm to recreate pictures, comes in two flavours, dots and triangles


How to use :

* Install ocaml
* Compile the images module using "ocamlc -c images.mli" then "ocamlc -c images.ml"
* Compile either the dots or the triangles version using "ocamlc -o dots unix.cma graphics.cma images.cmo dots.ml" or "ocamlc -o triangles unix.cma graphics.cma images.cmo triangles.ml"
* Use either ./dots or ./triangles on any picture of your choice (the smaller, the better)
