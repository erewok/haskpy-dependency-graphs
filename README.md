# haskpy-dependency-graphs
An attempt to build Python dependency graphs in Haskell

## WORK IN PROGRESS
This application was inspired by two things:

1. An interest in learning Haskell

2. A desire to map out dependencies in Python applications (and to find circular dependencies).

This project has just been started (and may not ever be completed), but in an effort to save current progress, it is being hosted here.


## Problem Areas

* ~~Function `displayGraph` relies on route to index template. This won't work outside of source directory!~~

* There is no real attempt at error handling at this point, and there are likely to be some errors...

* Force graph may be a poor way to represent this data structure

* Nothing from any `__init__.py` module is being examined

* No performance analysis has been done. This may not work for larger applications

* Graphs are not that useable at the moment: need to tweak D3 display.

* Graphs do not mark cycles, ~~or direction of edges.~~

* Functions that examine modules may fail on missing files.

* Will not work for Python modules with multiline imports.

* And more...

## How to Use

Installing dependency-graphs will create a `pydependencies` executable. In order to use it, you must have a `PYTHONPATH` declared (without a `PYTHONPATH` declared, the program will error out).

To invoke the program, after you have declared a PYTHONPATH that includes the module you care about, pass in the Python version and the file you would like to examine:

```
    pydependencies "python3.4" ./python_module > output.html
```

This will generate an `output.html` file, which can be opened in a web browser.

The `pydependencies` application will, by default, ignore all **third-party** and **python standard library** modules. Thus, if you try to examine a Python module inside a `site-packages` directory, the output will be empty.