# haskpy-dependency-graphs
An attempt to build Python dependency graphs in Haskell

## WORK IN PROGRESS
This application was inspired by two things:

1. An interest in learning Haskell

2. A desire to map out dependencies in Python applications (and to find circular dependencies).

This project has just been started (and may not ever be completed), but in an effort to save current progress, it is being hosted here.


## Problem Areas

    * Force graph may be a poor way to represent this data structure

    * Nothing from any `__init__.py` module is being examined

    * No performance analysis has been done. This may not work for larger applications

    * Graphs are not that useable at the moment: need to tweak D3 display.

    * Graphs do not mark cicles, or direction of edges.

    * Function `displayGraph` relies on route to index template. This won't work outside of source directory.

    * Functions that examine modules may fail on missing files.

    * And more...